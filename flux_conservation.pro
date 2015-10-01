;-----------------------------------------------------------------------
; module:	flux_conservation
; date:		Jul 12th, 2007
;
; author:	Heinke Frerichs
; email:	hfrerichs@wisc.edu
;
; description:	provides routines to plot flux conservation
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro read_flux_conservation, lun
        common  grid_info_cb
	common	config_cb
        common  flux_conservation_cb, $
                RadA, RadB, $   ; offsets for radial range containing plasma cells
                PolA, PolB, $   ; offsets for poloidal range containing plasma cells
                FC_size, $      ; size of each subzone with plasma cells
                FC_os, $        ; offset array for each subzone
                FC_data         ; flux conservation data

        ; define offsets for cells with plasma
        FC_size         = make_array(nzonet  , /long)
        FC_os           = make_array(nzonet+1, /long)
        FC_os(0)        = 0L
	RadA		= make_array(nzonet  , /long)
	RadB		= make_array(nzonet  , /long)
	PolA		= make_array(nzonet  , /long)
	PolB		= make_array(nzonet  , /long)
        for iz=0,nzonet-1 do begin
        	RadA(iz)	= r_surf_pl_trans_range(0,iz)
        	RadB(iz)	= r_surf_pl_trans_range(1,iz)+1
        	PolA(iz)	= p_surf_pl_trans_range(0,iz)
        	PolB(iz)	= p_surf_pl_trans_range(1,iz)+1
        	;RadA(iz)	= 1
        	;RadB(iz)	= srf_radi(iz)-1
                FC_size(iz)     = (RadB(iz)-RadA(iz)-1) * (PolB(iz)-PolA(iz)-1)
                FC_os(iz+1)     = FC_os(iz) + FC_size(iz)
        endfor
	print, 'FC_size = ', FC_size

        ; prepare array for flux conservation data
        FC_data = make_array (FC_os(nzonet), /fl)

        ; loading flux conservation data
        ;openr, lun, fc_file, /get_lun
        print, 'loading flux conservation data'
	print, nzonet
        for iz=0,nzonet-1 do begin
                dummy   = make_array(FC_size(iz), /fl)
                readf, lun, dummy
                FC_data(FC_os(iz):FC_os(iz+1)-1)        = dummy
        endfor

end; pro read_flux_conservation
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro plot_flux_conservation
	common	grid_info_cb
	common	grid_data_cb
	common	flux_conservation_cb
	common	plot_cb
	common	config_cb
	common	event_id_cb

	machine_id	= get_selected_machine(/get_id)
	case (machine_id) of
	0:	s_use		= s_d3d
	1:	s_use		= s_textor
	else:	s_use		= s_default
	endcase

	; make cut at toroidal angle phi
	phi	= double(get_entry(phi_select_id))
	poloidal_cut, phi, cut_grid

	case coords of
	0: begin
		xg_arr	= cut_grid.rg
		yg_arr	= cut_grid.zg
		pl_use	= s_use.X_RZ
	end
	1: begin
		xg_arr	= cut_grid.thetag
		yg_arr	= cut_grid.rming
		pl_use	= s_use.X_ThRmin
	end
	2: begin
		xg_arr	= cut_grid.thetag
		yg_arr	= cut_grid.psig
		pl_use	= s_use.X_ThPsi
	end
	endcase

	; plotting range
	;xrange	= [min(xg_arr), max(xg_arr)]
	;yrange	= [min(yg_arr), max(yg_arr)]
	xmin	= min(xg_arr)
	str	= get_entry('xmin')
	if str ne '' then xmin	= double(str)

	xmax	= max(xg_arr)
	str	= get_entry('xmax')
	if str ne '' then xmax	= double(str)

	ymin	= min(yg_arr)
	str	= get_entry('ymin')
	if str ne '' then ymin	= double(str)

	ymax	= max(yg_arr)
	str	= get_entry('ymax')
	if str ne '' then ymax	= double(str)

	xrange	= [xmin, xmax]
	yrange	= [ymin, ymax]

	xsize	= 800
	ysize	= 600
	xysize	= [800, 600]
	iflag	= open_plot('', xrange, yrange, xysize=xysize)
	if (iflag eq -1) then return


	; data range
	zmin    = min(FC_data)
	str	= (get_entry('dmin'))[0]
	if str ne '' then zmin	= double(str)

	zmax    = max(FC_data)
	str	= (get_entry('dmax'))[0]
	if str ne '' then zmax	= double(str)


	xarr    = make_array (5, /fl)
	yarr    = make_array (5, /fl)

	for iz=SubZone_OS(cut_grid.itz),SubZone_OS(cut_grid.itz+1)-1 do begin
	for i=RadA(iz),RadB(iz)-2 do begin
	for j=PolA(iz),PolB(iz)-2 do begin
		ig      = i + (j+cut_grid.it*(srf_polo(iz)-1))*(srf_radi(iz)-1)  +  mesh_p_os(iz)
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

		if ((coords eq 1 or coords eq 2)  and  (max(xarr) - min(xarr)) ge 20) then begin
			for k=0,4 do begin
				if (xarr(k) lt 180) then xarr(k) = xarr(k) + 360
			endfor
		endif

		;ifc     = FC_os(iz) + i-RadA(iz) + j * (RadB(iz) - RadA(iz) - 1)
		ifc     = FC_os(iz) + (i-RadA(iz)) * (PolB(iz)-PolA(iz)-1) + (j-PolA(iz))
		znn     = round(n_color * (FC_data(ifc) - zmin) / (zmax - zmin))
		if (FC_data(ifc) gt zmax) then znn = n_color
		if (FC_data(ifc) lt zmin) then znn = 0
		polyfill, xarr, yarr, col=znn, /data, noclip=0
		if (plot_grid) then oplot, xarr, yarr, col=255
	endfor
	endfor
	endfor

;.......................................................................
; oplot limiter
	;wall_file	= 'wall_RZ.dat'
	;n		= file_lines(wall_file)-1
	;wall_data	= make_array(2, n, /fl)

	;str	= ''
	;openr, lun, wall_file, /get_lun
	;readf, lun, str
	;readf, lun, wall_data
	;free_lun, lun

	;wall_data	= wall_data * 100
	;oplot, wall_data(0,*), wall_data(1,*)
;.......................................................................



; color bar
	level = (zmin+findgen(n_color+1)/n_color*(zmax-zmin))
	color = findgen(n_color+1)

	bar_dummy = [transpose(level),transpose(level)]
	xbar = findgen(2)

	CB_text = magnetic_arr[2,0]
	contour, bar_dummy, xbar, level, $
		/FILL, C_COLORS=color,LEVELS=level,YSTYLE=1,XSTYLE=4,YTITLE=$
		CB_text, /NOERASE,XMARGIN=[128,2],TICKLEN=0.2


	close_plot
end; pro plot_flux_conservation
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; init flux_conservation routines
pro flux_conservation
end; pro flux_conservation
;-----------------------------------------------------------------------
