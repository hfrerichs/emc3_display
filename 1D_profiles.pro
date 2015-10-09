;-----------------------------------------------------------------------
; module:	1D_profiles
; date:		Jan 18th, 2011
;
; author:	Heinke Frerichs
; email:	hfrerichs@wisc.edu
;
; description:	provides processing and plotting routines for 1D profiles
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro generate_coords, coords
	; define structure for coordinates of one point
	coords	= { t_coords, $
			x:0.0, $
			ir:0, ip:0, it:0, iz:0,	$
			r:0.0, s:0.0, t:0.0,	$
			id8:[0L,0L,0L,0L,0L,0L,0L,0L]	$
		  }
end; pro generate_coords, coords
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; load precalculated set of coordinates from file 'data_file' and
; return values in 'coordinates'
;-----------------------------------------------------------------------
pro load_coordinates_for_1D_profile, data_file
	common	plot_1D_cb, $
		coords_1D, n_coords_1D

	generate_coords, tmp0

	; scan data file for number of points
	print, 'loading coordinates from file: ', data_file
	result	= QUERY_ASCII(data_file, info)
	if (result ne 1) then begin
		;print, 'error in QUERY_ASCII on file:', data_file
		;stop
		print, 'file empty!'
		n_coords_1D	= 0
		return
	endif
	n_data	= info.lines
	n_coords_1D	= n_data

	; read coordinates from data file
	coords_1D	= replicate (tmp0, n_data)
	openr, lun, data_file, /get_lun
	readf, lun, coords_1D
	free_lun, lun
end; pro load_coordinates_for_1D_profile
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; find available plot types for selected machine and shot
;-----------------------------------------------------------------------
function get_1D_plot_types
	common	config_cb
	machine		= get_selected_machine()
	shot		= get_selected_shot()

	str_array	= file_basename(file_search(sim_dir+'/'+machine+'/'+shot+'/'+geometry_dir+'/1D_plots/*.dat'), '.dat')
	new_str		= 'Generate new profile'

	if size(str_array, /dimensions) eq 0 then begin
		str_array = [new_str]
	endif else begin
		str_array = [new_str, str_array]
	endelse

	return, str_array
end; function get_1D_plot_types
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; update list of available plot types (i.e. precalculated sample points)
; to be called after machine / shot has changed
;-----------------------------------------------------------------------
pro update_1D_plot_types
	id	= widget_info(1L, find_by_uname='load_1D_id')
	widget_control, id, set_value=get_1D_plot_types()
end; pro update_1D_plot_types
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; set/unset sensitivity of window elements for the generation of new profile types
; to be called after the selection of combobox 'load_1D_id' has changed
;-----------------------------------------------------------------------
pro update_1D_plot_window
	common	config_cb

	type_sel	= get_combobox_text('load_1D_id')
	sensitive	= 0

        c_id		= widget_info(1L, find_by_uname='1D_profile_text')
	str		= ''
	widget_control, c_id, set_value=str, /no_newline

	if (type_sel eq 'Generate new profile') then begin
		sensitive = 1
	endif else begin
		; set info text for selected profile (if info file is present)
		machine		= get_selected_machine()
		shot		= get_selected_shot()
		info_file = sim_dir+'/'+machine+'/'+shot+'/'+geometry_dir+'/1D_plots/'+type_sel+'.info'
		if file_test(info_file) then begin
			openr, lun, info_file, /get_lun
			while ~ eof(lun) do begin
				readf, lun, str
				widget_control, c_id, set_value=str, /append
			endwhile
			free_lun, lun
		endif
	endelse
        widget_control, c_id, editable=sensitive

	; set/unset sensitivity
        widget_control, widget_info(1L, find_by_uname='lrad_tangle'), sensitive=sensitive
        widget_control, widget_info(1L, find_by_uname='rad_tangle'), sensitive=sensitive

        widget_control, widget_info(1L, find_by_uname='l_1D_R'), sensitive=sensitive
        widget_control, widget_info(1L, find_by_uname='t_1D_R1'), sensitive=sensitive
        widget_control, widget_info(1L, find_by_uname='t_1D_R2'), sensitive=sensitive
        widget_control, widget_info(1L, find_by_uname='l_1D_Z'), sensitive=sensitive
        widget_control, widget_info(1L, find_by_uname='t_1D_Z1'), sensitive=sensitive
        widget_control, widget_info(1L, find_by_uname='t_1D_Z2'), sensitive=sensitive

        widget_control, widget_info(1L, find_by_uname='lsave_1D'), sensitive=sensitive
        widget_control, widget_info(1L, find_by_uname='save_1D'), sensitive=sensitive
end; pro update_1D_plot_window
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; pre-sample coordinates for 1D profile
;-----------------------------------------------------------------------
pro generate_new_1D_profile
	common	config_cb
	common	plot_1D_cb
	common	grid_info_cb
	common	grid_data_cb


	n_coords_1D	= 101
	generate_coords, coords
	coords_1D	= replicate (coords, n_coords_1D)

; get user input values for toroidal and poloidal position
	phi	= (float(get_entry('rad_tangle')))(0)
	R1	= (float(get_entry('t_1D_R1')))(0)
	R2	= (float(get_entry('t_1D_R2')))(0)
	Z1	= (float(get_entry('t_1D_Z1')))(0)
	Z2	= (float(get_entry('t_1D_Z2')))(0)
	DR	= R2 - R1
	DZ	= Z2 - Z1
	DL	= sqrt(DR^2 + DZ^2)

; calculate poloidal cut
	poloidal_cut, phi, cut_grid, iflag
	if (iflag eq -1) then begin
		print, 'error in poloidal_cut for phi = ', phi
		return
	endif
	itz	= cut_grid.itz
	it	= cut_grid.it
	t	= cut_grid.t

	print, 'Generating new profile ...'
	i_coord = 0
	for i=0,n_coords_1D-1 do begin
		print, i
		;Rin	=  100.0 + 0.5*i
		;Zin	= -117.5
		s = 1.0 * i / n_coords_1D
		Rin	= R1 + DR * s
		Zin	= Z1 + DZ * s
		L	= DL * s
		find_cell, Rin, Zin, itz, iz, it, ip, ir, id, r, s, t, ierr
		if (ierr eq 0) then begin
			ig      = ir + (ip+it*(srf_polo(iz)-1))*(srf_radi(iz)-1)  +  mesh_p_os(iz)
			ic	= idcell(ig) - 1
			if (ic ge nc_pl or ic lt 0) then continue

			coords_1D[i_coord].x   = L
			coords_1D[i_coord].ir  = ir
			coords_1D[i_coord].ip  = ip
			coords_1D[i_coord].it  = it
			coords_1D[i_coord].iz  = iz
			coords_1D[i_coord].r   = r
			coords_1D[i_coord].s   = s
			coords_1D[i_coord].t   = t
			coords_1D[i_coord].id8 = id
			i_coord = i_coord + 1
		endif
	endfor
	n_coords_1D	= i_coord
	print, '... done'

	; save coordinates to 
	save_file	= get_entry('save_1D')
	if save_file ne '' then begin
		machine		= get_selected_machine()
		shot		= get_selected_shot()
		dir		= sim_dir+'/'+machine+'/'+shot+'/'+geometry_dir+'/1D_plots'
		if ~ file_test(dir, /directory) then file_mkdir, dir
		file_base	= dir+'/'+save_file
		data_file	= file_base+'.dat'
		openw, lun, data_file, /get_lun
		printf, lun, coords_1D[0:i_coord-1], format='(f10.5,4i6,3f10.5,8i8)'
		free_lun, lun

		; save profile information provided by user
		info_txt	= get_entry('1D_profile_text')
		if (info_txt[0] ne '') then begin
			info_file	= file_base+'.info'
			openw, lun, info_file, /get_lun
			printf, lun, info_txt
			free_lun, lun
		endif
		update_1D_plot_types
	endif
end; pro generate_new_1D_profile
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; main plot routine for 1D profiles
;-----------------------------------------------------------------------
pro plot_1D_profile, physQ, run_title, id0, sub_id
	common	plot_1D_cb
	common	config_cb
	common	grid_info_cb
	common	grid_data_cb

	type_sel	= get_combobox_text('load_1D_id')
	
	if (type_sel eq 'Generate new profile') then begin
		generate_new_1D_profile
	endif else begin
		machine		= get_selected_machine()
		shot		= get_selected_shot()
		data_file	= sim_dir+'/'+machine+'/'+shot+'/'+geometry_dir+'/1D_plots/'+type_sel+'.dat'
		load_coordinates_for_1D_profile, data_file
	endelse

	;print, 'mapping cell averaged data to grid nodes ...'
	cell_to_corner, physQ, physQ_corner
	fn		= make_array(8, /fl)

	print, 'sampling ',n_coords_1D,' data points for profile ',type_sel,' ...'
	openw, lun, 'test_profile.dat', /get_lun
	for i=0,n_coords_1D-1 do begin
		id8	= coords_1D[i].id8
		ir	= coords_1D[i].ir
		ip	= coords_1D[i].ip
		it	= coords_1D[i].it
		iz	= coords_1D[i].iz
		r	= coords_1D[i].r
		s	= coords_1D[i].s
		t	= coords_1D[i].t
		x	= coords_1D[i].x

		fn	= physQ_corner(id8)
		ig      = ir + (ip+it*(srf_polo(iz)-1))*(srf_radi(iz)-1)  +  mesh_p_os(iz)
		ic	= idcell(ig) - 1

		Psi	= femint(psin(id8), r, s, t)
		res	= femint(fn, r, s, t)

		printf, lun, x, Psi, res, physQ(ic)
	endfor
	free_lun, lun
	print, '... done'
end; pro plot_1D_profile, physQ, run_title, id0, sub_id
;-----------------------------------------------------------------------
