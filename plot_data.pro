;-----------------------------------------------------------------------
; module:	plot_data
; date:		Jul 12th, 2007
;
; author:	Heinke Frerichs
; email:	h.frerichs@fz-juelich.de
;
; description:	provides plotting routines
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
FORWARD_FUNCTION read_scenario

pro read_plot_config, conf_file
	common	plot_cb, $
		xl_margin, xr_margin, xl_cb_margin, xr_cb_margin, xu_cb_margin, xd_cb_margin, n_color, $
		coords, type, plot_grid, animated, char_size, char_thick, $
		data_file, file_type, pl_use, s_textor, s_d3d, s_default

	; plot settings
	t_margin	= {t1, XL:0.0, XR:0.0, YU:0.0, YD:0.0}	; template for margin settings
	t_window	= {t2, xsize:0.0, ysize:0.0}		; template for window size settings
	t_char		= {t3, csize:0.0, cthick:0.0}		; template for character size and thickness

	scenario	= { t_scenario, $
				win		: t_window, $	; size of plot window
				pl_margin	: t_margin, $	; margins for data plot
				cb_margin	: t_margin, $	; margins for color bar
				char		: t_char $
			  }

	setting		= {t_setting, $
				X_RZ		: scenario, $	; X window, R-Z plot
				X_ThRmin	: scenario, $	; X window, theta-rmin plot
				X_ThPsi		: scenario, $	; X window, theta-psi plot
				EPS_RZ		: scenario, $	; EPS, R-Z plot
				EPS_ThRmin	: scenario, $	; EPS, theta-rmin plot
				EPS_ThPsi	: scenario  $	; EPS, theta-psi plot
			  }

	s_textor	= setting
	s_d3d		= setting
	pl_use		= scenario

	print, 'reading plot configurations from file: ', conf_file
	str	= ''
	openr, lun, conf_file, /get_lun

	s_textor.X_RZ		= read_scenario(lun, s_type)
	s_textor.X_ThRmin	= read_scenario(lun, s_type)
	s_textor.X_ThPsi	= read_scenario(lun, s_type)
	s_textor.EPS_RZ		= read_scenario(lun, s_type)
	s_textor.EPS_ThRmin	= read_scenario(lun, s_type)
	s_textor.EPS_ThPsi	= read_scenario(lun, s_type)
	s_d3d.X_RZ		= read_scenario(lun, s_type)
	s_d3d.X_ThRmin		= read_scenario(lun, s_type)
	s_d3d.X_ThPsi		= read_scenario(lun, s_type)
	s_d3d.EPS_RZ		= read_scenario(lun, s_type)
	s_d3d.EPS_ThRmin	= read_scenario(lun, s_type)
	s_d3d.EPS_ThPsi		= read_scenario(lun, s_type)
	s_default	= s_textor

	free_lun, lun
end; read_plot_config
;.......................................................................
function read_scenario, lun, s_type
	str	= scrape(lun, cmask='#')

	s_type	= strmid(str,0,16)
	str_new	= strmid(str,16)
	reads, str_new, xsize, ysize, plXL, plXR, plYU, plYD, cbXL, cbXR, cbYU, cbYD, csize, cthick

	scenario	= {t_scenario}
	scenario.win.xsize	= xsize
	scenario.win.ysize	= ysize
	scenario.pl_margin.XL	= plXL
	scenario.pl_margin.XR	= plXR
	scenario.pl_margin.YU	= plYU
	scenario.pl_margin.YD	= plYD
	scenario.cb_margin.XL	= cbXL
	scenario.cb_margin.XR	= cbXR
	scenario.cb_margin.YU	= cbYU
	scenario.cb_margin.YD	= cbYD
	scenario.char.csize	= csize
	scenario.char.cthick	= cthick
	return, scenario
end; read_scenario
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro set_plot_option, opt, coords=coords_, plot_grid=plot_grid_, animated=animated_
	common	plot_cb

	if keyword_set(coords_)		then coords=opt
	if keyword_set(plot_grid_)	then plot_grid=opt
	if keyword_set(animated_)	then animated=opt
end; pro plot_option
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro get_plot_settings
	common	plot_cb
	common	config_cb

	if copy_data(x,'win_xsize') then pl_use.win.xsize = x
	if copy_data(x,'win_ysize') then pl_use.win.ysize = x

	if copy_data(x,'plXL') then pl_use.pl_margin.XL = x
	if copy_data(x,'plXR') then pl_use.pl_margin.XR = x
	if copy_data(x,'cbXL') then pl_use.cb_margin.XL = x
	if copy_data(x,'cbXR') then pl_use.cb_margin.XR = x
end;
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro plot_window_event, event
	common	event_id_cb

	event_id	= widget_info(event.id, /uname)

	;print, event
	if (event.press) then begin
		print, event.x, event.y
	end
end; plot_window_event
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
function open_plot, title_str, xrange, yrange, xtitle, ytitle, xysize=xysize
	common	grid_data_cb
	common	plot_cb

	print, 'plotting ', title_str

	xu_cb_margin	= 2;	default values
	xd_cb_margin	= 4
; output to file?
	if (check_button('save_output')) then begin
		; extract last part of filename to determine filetype
		data_file	= get_entry('output_file')
		strs		= strsplit(data_file, '.', count=count, /extract)
		if (count eq 0) then begin
			print, "couldn't determine file type for file: ", data_file
			return, -1
		endif
		file_type	= strlowcase(strs(count-1))
		case (file_type) of

		'ps': begin
		; fullsize postscript
			print, 'to postscript file: ', data_file
			set_plot, 'PS'
			device, file=data_file, /color, bits=8, font_size=10, /LANDSCAPE
			xl_margin	=   5
			xr_margin	=  15
			xl_cb_margin	= 125
			xr_cb_margin	=   2
		end

		'eps': begin
		; encapsulated postscript

			id		= widget_info(1L, find_by_uname='cb_eps_formats')
			widget_control, id, get_value=eps_formats
			val		= widget_info(id, /combobox_gettext)
			i		= where(eps_formats eq val, count)
			case i of
			0: begin;====== 1 x 2 =============
				xl_margin	=   6
				xr_margin	=  12
				xl_cb_margin	=  50
				xr_cb_margin	=   2
				xsize		=  12
				ysize		=  18
				xstyle		=   1
				ystyle		=   1
			end
			1: begin;====== 1 x 1 =============
				xl_margin	=   6
				xr_margin	=  12
				xl_cb_margin	=  50
				xr_cb_margin	=   2
				xsize		=  12
				ysize		=   9
				xstyle		=   1
				ystyle		=   1
			end
			2: begin;====== 5 x 4 =============
				xl_margin	=   8
				xr_margin	=  15
				xl_cb_margin	=  59
				xr_cb_margin	=   2
				xsize		=  14
				ysize		=   9
				xstyle		=   1
				ystyle		=   1
			end
			3: begin;====== 1 x 2 (no boundary)
			; Full RZ plot for DIII-D with color bar in center
				xl_margin	=   0	; no boundary
				xr_margin	=   0
				xl_cb_margin	=  22
				xr_cb_margin	=  21
				xu_cb_margin	=  15	;char_size 1
				xd_cb_margin	=  18
			;xu_cb_margin	=   7	;char_size 1.6
			;xd_cb_margin	=  13	;1.6
			;xl_cb_margin	=  15	;1.6
			;xr_cb_margin	=  12	;1.6
				xsize		=  10
				ysize		=  20
				xstyle		=   5
				ystyle		=   5
			end
			endcase

			; Full RZ plot for DIII-D with color bar in center
;			xl_margin	=   0	; no boundary
;			xr_margin	=   0
;			xl_cb_margin	=  22
;			xr_cb_margin	=  21
;			xu_cb_margin	=  10	;char_size 1
;			xd_cb_margin	=  18
			;xu_cb_margin	=   7	;char_size 1.6
			;xd_cb_margin	=  13	;1.6
			;xl_cb_margin	=  15	;1.6
			;xr_cb_margin	=  12	;1.6
;			xsize		=  10
;			ysize		=  19
;			xstyle		=   5
;			ystyle		=   5

			;xl_margin	=   8	; with boundary
			;xr_margin	=   1
			;xl_cb_margin	=  25
			;xr_cb_margin	=  18
			;xu_cb_margin	=   8
			;xd_cb_margin	=  16
			;xsize		=  10
			;ysize		=  15
			;xstyle		=   1
			;ystyle		=   1


			; new automatic
			;xsize		=  20
			;ysize		=  12
			;xl_margin	=   8
			;xr_margin	=  14
			;xl_cb_margin	=  xsize*3.85
			;xr_cb_margin	=   2
			;xstyle		=   1
			;ystyle		=   1
			;char_size	= 1.1

		;xl_cb_margin	= pl_use.cb_margin.XL
		;xr_cb_margin	= pl_use.cb_margin.XR

			print, 'to eps file: ', data_file
			set_plot, 'PS'
			device, file=data_file, /ENCAPSUL, $
                        ;/COLOR,BITS=8,FONT_SIZE=12,xsize=12,ysize=09
                        ;/COLOR,BITS=8,FONT_SIZE=12,xsize=12,ysize=12
                        ;/COLOR,BITS=8,FONT_SIZE=9,xsize=12,ysize=09
                        ;/COLOR,BITS=8,FONT_SIZE=9,xsize=12,ysize=15
                        ;/COLOR,BITS=8,FONT_SIZE=12,xsize=12,ysize=15
                        /COLOR,BITS=8,FONT_SIZE=12,xsize=xsize,ysize=ysize
			;;xl_cb_margin	=  69

		end

		'txt': begin
		; plain data
			openw, lun, data_file, /get_lun
			printf, lun, '# x	y	data'
			data_file	= lun
		end

		else: begin
			print, 'unkown file type: ', file_type
			return, -1
		end
		endcase

		char_thick	= 3.0
		char_size	= 1.0
	endif else begin
	; open X window for plotting
		file_type	= 'X'
		set_plot, 'X'
		device, retain=2, decomposed=0
		;base		= widget_base(/row, xsize=xysize(0), /base_align_center, title=title_str, uname='plot_window')
		;plot_window	= widget_draw(base,xsize=xysize(0) ,ysize=xysize(1),/button_events)
		base		= widget_base(/row, xsize=pl_use.win.xsize, /base_align_center, title=title_str, uname='plot_window')
		plot_window	= widget_draw(base,xsize=pl_use.win.xsize ,ysize=pl_use.win.ysize,/button_events)
		widget_control, base, /realize

		;xl_margin	=   7
		;xr_margin	=  20
		;xl_cb_margin	= 128
		;xr_cb_margin	=   2
		xl_margin	= pl_use.pl_margin.XL
		xr_margin	= pl_use.pl_margin.XR
		xl_cb_margin	= pl_use.cb_margin.XL
		xr_cb_margin	= pl_use.cb_margin.XR

		;char_thick	= 1.0
		;char_size	= 1.0
		char_thick	= pl_use.char.cthick
		char_size	= pl_use.char.csize

		xstyle		=   1
		ystyle		=   1
	endelse

	n_color		= 250


	; prepare plotting window for RZ-plot
	;xmin   = 120
	;xmay   = 140
	;ymin   = -110
	;ymax   = -90

	;title_str	= 'electron temperature (preliminary result)'
	;title_str	= 'Electron Temperature'
	;print, xrange
	;print, yrange
	if (file_type eq 'txt') then return, 1

	tvlct, ct_red, ct_green, ct_blue, /GET
	ct_red(0)	= 0
	ct_green(0)	= 0
	ct_blue(0)	= 0
	ct_red(255)	= 255
	ct_green(255)	= 255
	ct_blue(255)	= 255
	tvlct, ct_red, ct_green, ct_blue

	plot, xrange, yrange, xstyle=xstyle, ystyle=ystyle, /nodata, xmargin=[xl_margin,xr_margin], title=title_str, $
		xtitle=xtitle, ytitle=ytitle, charsize=char_size, charthick=char_thick

	return, 1
end; function open_plot
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro close_plot
	common	plot_cb

	switch (file_type) of
	'ps':
	'eps':	begin
	 	device, /close
		break
	end

	'txt':	free_lun, data_file

	else:
	endswitch

	print, '... done'
end; pro close_plot
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro plot_poloidal_cut, physQ, run_title, id0, sub_id
	common	plot_cb
	common	grid_info_cb
	common	grid_data_cb
	common	event_id_cb
	common	config_cb


; window size ..........................................................
	machine_id	= get_selected_machine(/get_id)
	;print, 'machine_id = ', get_selected_machine(/get_id)

	case (machine_id) of
	;0:	plot_pref	= d3d_pref
	;1:	plot_pref	= textor_pref
	;else:	plot_pref	= default_pref
	0:	s_use		= s_d3d
	1:	s_use		= s_textor
	else:	s_use		= s_default
	endcase


; prepare output for animation
;	if (animated) then begin
;		iphi1 = 0
;		iphi2 = 120
;		phi1	= phi_plane(0)
;		phi2	= phi_plane(phi_pl_os(nzonet)-1)
;		print, 'phi1 -> phi2 [deg] = ', phi1, phi2
;	endif else begin
;		iphi1 = 0
;		iphi2 = 0
;		phi1	= double(get_entry(phi_select_id))
;		phi2	= phi1
;	endelse
;	for iphi=iphi1,iphi2 do begin
;	phi	= phi1
;	if (iphi gt iphi1) then phi = phi1 + (phi2 - phi1) * (iphi - iphi1) / (iphi2 - iphi1)
;	;phi = phi1 + s
;	print, 'phi [deg] = ', phi
	;phi	= double(get_entry(phi_select_id))

; make cut at toroidal angle phi .......................................
	phi	= double(get_entry(phi_select_id))
	poloidal_cut, phi, cut_grid, iflag


; prepare plot-coordinates dependant stuff .............................
	case coords of
	0: begin
		xg_arr	= cut_grid.rg
		yg_arr	= cut_grid.zg
		;xysize	= plot_pref.RZsize
		pl_use	= s_use.X_RZ
		xtitle	= 'R [cm]'
		ytitle	= 'Z [cm]'
	end
	1: begin
		xg_arr	= cut_grid.thetag
		yg_arr	= cut_grid.rming
		;xysize	= plot_pref.trsize
		pl_use	= s_use.X_ThRmin
		xtitle	= 'Poloidal Angle [deg]'
		ytitle	= 'Minor Radius [cm]'
	end
	2: begin
		xg_arr	= cut_grid.thetag
		yg_arr	= cut_grid.psig
		;xysize	= plot_pref.tpsize
		pl_use	= s_use.X_ThPsi
		xtitle	= 'Poloidal Angle [deg]'
		;ytitle	= 'Psi_N'
		ytitle	= 'Poloidal Flux (normalized)'
	end
	endcase


; plot title and axis labels ...........................................
	case (id0) of
	0: begin
		phys_title	= plasma_arr(0,sub_id)
		phys_label	= plasma_arr(2,sub_id)
		unit_str	= plasma_arr(3,sub_id)
		;CB_text		= plasma_arr(2,sub_id)+' ['+plasma_arr(3,sub_id)+']'
		cell_index	= 'p'
	end
	2: begin
		phys_title	= magnetic_arr(0,sub_id)
		phys_label	= magnetic_arr(2,sub_id)
		unit_str	= magnetic_arr(3,sub_id)
		;CB_text 	= magnetic_arr(2,sub_id)+' ['+magnetic_arr(3,sub_id)+']'
		cell_index	= 'p'
	end
	3: begin
		phys_title	= diagnostic_arr(0,sub_id)
		phys_label	= diagnostic_arr(2,sub_id)
		unit_str	= diagnostic_arr(3,sub_id)
		;CB_text 	= diagnostic_arr(2,sub_id)+' ['+diagnostic_arr(3,sub_id)+']'
		cell_index	= diagnostic_arr(4,sub_id)
	end
	4: begin
		phys_title	= impurity_arr(0,sub_id)
		phys_label	= impurity_arr(2,sub_id)
		unit_str	= impurity_arr(3,sub_id)
		;CB_text		= impurity_arr(2,sub_id)+' ['+impurity_arr(3,sub_id)+']'
		cell_index	= 'p'
	end
	endcase

	; set plot title
	title_str	= phys_title + ' ' + run_title
	tmp	= get_entry('plot_title')
	if (tmp ne '') then title_str = tmp

	; set quantity label and units
	CB_text		= phys_label
	if (unit_str ne '') then CB_text = CB_text + ' [' + unit_str + ']'
	tmp	= get_entry('plot_units')
	if (tmp ne '') then CB_text = tmp


; plot data definition .................................................
	case (cell_index) of
	'p': ncell_max = nc_pl
	'n': ncell_max = ncell_n1
	'm': ncell_max = ncell_n1
	endcase


; plot range ...........................................................
	; automatic range
	switch coords of
	0: begin
		xmin	= 0.98 * min(xg_arr)
		xmax	= 1.02 * max(xg_arr)
		ymin	= 1.02 * min(yg_arr)
		ymax	= 1.02 * max(yg_arr)
		break
	end
	1: 
	2: begin
		xmin	=  -5
		xmax	= 365
		ymin	= max(yg_arr)
		ymax	= min(yg_arr)
		iarr	= make_array(4, /long)
		for iz=SubZone_OS(cut_grid.itz),SubZone_OS(cut_grid.itz+1)-1 do begin
		for i=1,zon_radi(iz)-2 do begin
		for j=0,zon_polo(iz)-1 do begin
			ig      = i + (j+cut_grid.it*zon_polo(iz))*zon_radi(iz)  +  mesh_p_os(iz)
			id	= idcell(ig) - 1
			;if (id ge nc_pl) then continue
			;if (id ge ncell_n1) then continue
			if (id ge ncell_max) then continue

			iarr(0)	= i + j*srf_radi(iz)  +  cut_grid.os(iz - SubZone_OS(cut_grid.itz))
			iarr(1) = iarr(0) + srf_radi(iz)
			iarr(2) = iarr(1) + 1
			iarr(3) = iarr(0) + 1
			ymin	= min([ymin, yg_arr(iarr)])
			ymax	= max([ymax, yg_arr(iarr)])
		endfor
		endfor
		endfor
		dy	= ymax - ymin
		ymin	= ymin - 0.02*dy
		ymax	= ymax + 0.02*dy
	end
	endswitch

	; user defined plot range
	if (check_button('set_range_id')) then begin
	str	= get_entry('xmin')
	if str ne '' then xmin	= double(str)

	str	= get_entry('xmax')
	if str ne '' then xmax	= double(str)

	str	= get_entry('ymin')
	if str ne '' then ymin	= double(str)

	str	= get_entry('ymax')
	if str ne '' then ymax	= double(str)
	endif

	xrange	= [xmin, xmax]
	yrange	= [ymin, ymax]

	; data range
	zmin    = min(physQ)
	str	= (get_entry('dmin'))[0]
	if str ne '' then zmin	= double(str)

	zmax    = max(physQ)
	str	= (get_entry('dmax'))[0]
	if str ne '' then zmax	= double(str)


	get_plot_settings
	;iflag	= open_plot(title_str, xrange, yrange, xtitle, ytitle, xysize=xysize)
	iflag	= open_plot(title_str, xrange, yrange, xtitle, ytitle)
	if (iflag eq -1) then return


; plot data ............................................................
	xarr    = make_array (5, /fl)
	yarr    = make_array (5, /fl)
	for iz=SubZone_OS(cut_grid.itz),SubZone_OS(cut_grid.itz+1)-1 do begin
	;for iz=SubZone_OS(cut_grid.itz),SubZone_OS(cut_grid.itz+1)-1,2 do begin		; 2009-08-28, skip plotting of private flux region
	for i=0,srf_radi(iz)-2 do begin
	for j=0,srf_polo(iz)-2 do begin
		ig      = i + (j+cut_grid.it*(srf_polo(iz)-1))*(srf_radi(iz)-1)  +  mesh_p_os(iz)
		id	= idcell(ig) - 1
		if (id ge ncell_max) then continue
		;if (id ge ncell_n1) then continue
		;if (id ge nc_pl) then continue
		if (id lt 0L) then continue

		i1      = i + j*srf_radi(iz)  +  cut_grid.os(iz - SubZone_OS(cut_grid.itz))
		i2      = i1 + srf_radi(iz)
		i3      = i2 + 1
		i4      = i1 + 1

		xarr(0) = xg_arr(i1)
		xarr(1) = xg_arr(i2)
		xarr(2) = xg_arr(i3)
		xarr(3) = xg_arr(i4)
		xarr(4) = xg_arr(i1)

		yarr(0) = yg_arr(i1)
		yarr(1) = yg_arr(i2)
		yarr(2) = yg_arr(i3)
		yarr(3) = yg_arr(i4)
		yarr(4) = yg_arr(i1)

		; fix cell coordinates when using poloidal angle for plotting
		if ((coords eq 1 or coords eq 2)  and  (max(xarr) - min(xarr)) ge 20) then begin
			for k=0,4 do begin
				if (xarr(k) lt 180) then xarr(k) = xarr(k) + 360
			endfor
		endif

		; data on physical or magnetic cell?
		case (cell_index) of
		'p': idx = id
		'n': idx = id
		'm': idx = ig
		endcase

		znn     = round((n_color-1) * (physQ(idx) - zmin) / (zmax - zmin)) + 1
		if (physQ(idx) ge zmax) then znn = n_color
		if (physQ(idx) le zmin) then znn = 1

		switch (file_type) of
		'ps':
		'eps':
		'X':	begin
		; graphical output
			polyfill, xarr, yarr, col=znn, /data, noclip=0
			if (plot_grid) then oplot, xarr, yarr, col=255
			break
		end
		'txt':	begin
		; text output
			xmid	= 0.25 * total(xarr(0:3))
			ymid	= 0.25 * total(yarr(0:3))
			;printf, data_file, xmid, ymid, physQ(idx)
			printf, data_file, xarr, yarr, physQ(idx)
		end
		endswitch
	endfor
	endfor
	endfor

; overlay data .........................................................
	overlay_data

; color bar ............................................................
	level = (zmin+findgen(n_color+1)/n_color*(zmax-zmin))
	color = findgen(n_color+1)

	bar_dummy = [transpose(level),transpose(level)]
	xbar = findgen(2)

	;print, 'zmin = ', zmin, ', zmax = ', zmax
	switch (file_type) of
	'ps':
	'eps':
	'X':	begin
	; graphical output
		contour, bar_dummy, xbar, level, $
			/FILL, C_COLORS=color,LEVELS=level,YSTYLE=1,XSTYLE=4,YTITLE=CB_text,$
			/NOERASE,xmargin=[xl_cb_margin,xr_cb_margin],ymargin=[xd_cb_margin,xu_cb_margin],$
			TICKLEN=0.2, charsize=char_size, charthick=char_thick
		break
	end
	endswitch


; close plot ...........................................................
	close_plot

; save output as png
;filename = FILEPATH('test'+strcompress(string(iphi, format='(i05)'), /remove_all)+'.png', /TMP)
;WRITE_PNG, filename, TVRD(/TRUE)
	;endfor

end; plot_poloidal_cut
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro plot_radial_profile, physQ, run_title, id0, sub_id
	common	grid_info_cb
	common	grid_data_cb

	plot_1D_profile, physQ, run_title, id0, sub_id
	return



; get user input values for toroidal and poloidal position
	phi	= (float(get_entry('rad_tangle')))(0)
	theta	= (float(get_entry('rad_pangle')))(0)
	print, 'radial profile for phi = ',phi,', theta = ',theta

; calculate poloidal cut
	poloidal_cut, phi, cut_grid, iflag
	if (iflag eq -1) then begin
		print, 'error in poloidal_cut for phi = ', phi
		return
	endif
	itz	= cut_grid.itz
	it	= cut_grid.it
	t	= cut_grid.t

; find poloidal cell index at inner boundary
	ip	= -1
	jp	= 0.0
	rmin_min	= max(cut_grid.rming)
	for iz=SubZone_OS(cut_grid.itz),SubZone_OS(cut_grid.itz+1)-1 do begin
	for j=0,srf_polo(iz)-2 do begin
		i1      = j*srf_radi(iz)  +  cut_grid.os(iz - SubZone_OS(itz))
		i2	= i1 + srf_radi(iz)
		theta1	= cut_grid.thetag(i1)
		theta2	= cut_grid.thetag(i2)
		if (theta2 - theta1 le -180) then theta1 = theta1 - 360

		rmin1	= cut_grid.rming(i1)
		rmin2	= cut_grid.rming(i2)
		if ((theta1 le theta) and (theta2 ge theta)) then begin
			s	= (theta - theta1) / (theta2 - theta1)
			rmin	= s * rmin2  +  (1-s) * rmin1
			if (rmin le rmin_min) then begin
				rmin_min	= rmin
				ip	= j
				jp	= s
			endif
		endif
	endfor
	endfor
; find poloidal cell index at outer boundary
	ip	= -1
	jp	= 0.0
	rmin_max	= min(cut_grid.rming)
	for iz=SubZone_OS(cut_grid.itz),SubZone_OS(cut_grid.itz+1)-1 do begin
	for j=0,srf_polo(iz)-2 do begin
		i1      = zon_radi(iz) + j*srf_radi(iz)  +  cut_grid.os(iz - SubZone_OS(itz))
		i2	= i1 + srf_radi(iz)
		theta1	= cut_grid.thetag(i1)
		theta2	= cut_grid.thetag(i2)
		if (theta2 - theta1 le -180) then theta1 = theta1 - 360

		rmin1	= cut_grid.rming(i1)
		rmin2	= cut_grid.rming(i2)
		if ((theta1 le theta) and (theta2 ge theta)) then begin
			s	= (theta - theta1) / (theta2 - theta1)
			rmin	= s * rmin2  +  (1-s) * rmin1
			if (rmin ge rmin_max) then begin
				rmin_max	= rmin
				ip	= j
				jp	= s
			endif
		endif
	endfor
	endfor


; prepare arrays
	cell_to_corner, physQ, physQ_corner
	fn		= make_array(8, /fl)
	n		= 101
	rad_data	= make_array(n, 5, /fl)

	;rmin_min	= 32.6
	;rmin_max	= 42.5
	drmin		= rmin_max - rmin_min
; calculate radial profile
	print, 'sampling radial profile with ', n, ' points:'
	;data_file	= get_entry('2D_output_file')
	data_file	= ''
	if (data_file eq '') then data_file = 'rad_profile.plt'
	openw, lun, data_file, /get_lun
	openw, lun2, 'sample_coordinates', /get_lun
	for i=0,n-1 do begin
		print, 'point ', i
		;Rin = 218.2 + 13.5 * i / (n-1)
		;Zin = 0.0
		Rin	= Rmag + (rmin_min + drmin * i/(n-1)) * cos(theta/180*!pi)
		Zin	= Zmag + (rmin_min + drmin * i/(n-1)) * sin(theta/180*!pi)
		find_cell, Rin, Zin, itz, iz, it, ip, ir, id, r, s, t, ierr
		if (ierr eq 0) then begin
			fn	= physQ_corner(id)
			ig      = ir + (ip+it*(srf_polo(iz)-1))*(srf_radi(iz)-1)  +  mesh_p_os(iz)
			ic	= idcell(ig) - 1
			if (ic ge nc_pl or ic lt 0) then begin
				;print, 'warning: skipping non plasma cell:', ic
				continue
			endif

			res	= femint(fn, r, s, t)
			Psi	= femint(psin(id), r, s, t)
			rad_data(i,0)	= 1
			rad_data(i,1)	= Rin
			rad_data(i,2)	= Psi
			rad_data(i,3)	= res
			rad_data(i,4)	= physQ(ic)
			printf, lun, Rin, Psi, res, physQ(ic)
			printf, lun2, ir, ip, it, iz, r, s, t, id, format='(4i6,3f10.5,8i8)'
		endif else begin
			rad_data(i,0) = -1
			print, 'warning: no cell found for point:'
			print, 'R = ',Rin,', Z = ',Zin
		endelse
	endfor
	free_lun, lun
	free_lun, lun2
	print, '... done'

; print profile
	xmin	= max(rad_data(*,2))
	xmax	= min(rad_data(*,2))
	ymin	= max(rad_data(*,3))
	ymax	= min(rad_data(*,3))
	for i=0,n-1 do begin
		if (rad_data(i,2) lt xmin  and  rad_data(i,0) eq 1) then xmin = rad_data(i,2)
		if (rad_data(i,2) gt xmax  and  rad_data(i,0) eq 1) then xmax = rad_data(i,2)
		if (rad_data(i,3) lt ymin  and  rad_data(i,0) eq 1) then ymin = rad_data(i,3)
		if (rad_data(i,3) gt ymax  and  rad_data(i,0) eq 1) then ymax = rad_data(i,3)
	endfor

	plot, [xmin, xmax], [ymin, ymax], /nodata
	for i=0,n-1 do begin
		if (rad_data(i,0) eq 1) then begin
			oplot, [rad_data(i,2)], [rad_data(i,3)], psym=3
		endif
	endfor
end; plot_radial_profile
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro plot_data, data_id, sub_id, species_id=species_id
	common	plot_cb
	;common	grid_info_cb
	;common	grid_data_cb
	common	config_cb

	grid_dir	= sim_dir+'/'+get_selected_machine()+'/'+get_selected_shot()
	load_grid, grid_dir

	; load data
	machine		= get_selected_machine()
	shot		= get_selected_shot()
	casex		= get_selected_case()
	run		= get_selected_run()
	load_data, machine, shot, casex, run, data_id, sub_id, physQ, species_id=species_id
	run_title	= '('+shot+': '+casex+', '+run+')'

	; plot difference between runs?
	if (check_button('compare_runs')) then begin
		cmp_case	= get_selected_case(/cmp)
		cmp_run		= get_selected_run(/cmp)
		load_data, machine, shot, cmp_case, cmp_run, data_id, sub_id, physQ2, species_id=species_id
		dphysQ		= physQ - physQ2

		; get selected difference type (i.e. absolute/relative difference)
		id		= widget_info(1L, find_by_uname='cb_cmp_diff')
		widget_control, id, get_value=diff_strs
		diff_val	= widget_info(id, /combobox_gettext)
		i		= where(diff_strs eq diff_val, count)
		case i of
		1:	physQ = dphysQ / physQ
		2:	physQ = dphysQ / physQ2
		3:	physQ = dphysQ / (physQ+physQ2) * 2
		else:	physQ = dphysQ
		endcase

		run_title	= '('+shot+': '+casex+', '+run+'  -  '+cmp_case+', '+cmp_run+')'
	endif

        id		= widget_info(1L, find_by_uname='plot_type_tab')
	plot_type	= widget_info(id, /tab_current)
;	get_plot_settings

	case plot_type of
; poloidal cut
	0: begin
		if (data_id eq 2 and sub_id eq 0) then begin
			plot_flux_conservation
			return
		endif
		plot_poloidal_cut, physQ, run_title, data_id, sub_id
	end
; radial profile
	1: begin
		plot_radial_profile, physQ, run_title, data_id, sub_id
	end
; field line profile
	2: begin
		plot_fl_profile, physQ
	end
	endcase
end; pro plot_data
;-----------------------------------------------------------------------
