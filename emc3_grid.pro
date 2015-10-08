;-----------------------------------------------------------------------
; module:	emc3_grid
; date:		Jul 10th, 2007
;
; author:	Heinke Frerichs
; email:	hfrerichs@wisc.edu
;
; description:	provides routines to read and plot the emc3 grid for a
;		selected shot
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; read grid information file
pro read_grid_info, grid_dir, iflag
	common	config_cb
	common	grid_info_cb, $
		v1, v2, nzonet, ntorzones, subzone_os, $
		srf_radi, srf_polo, srf_toro, phi_pl_os, mesh_p_os, grid_p_os, $
		zon_radi, zon_polo, zon_toro, $
		loaded_grid, r_surf_pl_trans_range, p_surf_pl_trans_range

	iflag = -1

; test if info file exists
	info_file	= grid_dir+'/'+geometry_dir+'/'+grid_info_file
	if not file_test(info_file) then begin
		print, 'grid information file not found: ', info_file
		return
	endif

	; open info file
	print, 'reading grid information from file: ', info_file
	str	= ''
	openr, lun, info_file, /get_lun
	readf, lun, str

; read version string
	v1	= 0
	v2	= 0
	if (strmid(str,2,34) eq 'geometry information file for EMC3') then begin
		v1	= fix(strmid(str,39,1))
		v2	= fix(strmid(str,41,2))
	endif
	
; read zone information
	nzonet	= 0
	
; read subzone information
	if (v1 ge 1) then begin
		NTorZones	= 0
		reads, scrape(lun), nzonet, NTorZones

		SubZone_OS	= make_array (NTorZones+1, /long)
		SubZone_OS(0)	= 0

		iz	= 0
		for i=1,NTorZones do begin
			reads, scrape(lun), iz
			SubZone_OS(i)	= SubZone_OS(i-1) + iz
		endfor

		if (SubZone_OS(NTorZones) ne nzonet) then begin
			print, 'SubZone_OS(NtorZones) .ne. nzonet'
			print, SubZone_OS(NtorZones), nzonet
			return
		endif

	endif else begin
		reads, scrape(lun), nzonet
		;NTorZones	= nzonet
		;SubZone_OS	= make_array (nzonet+1, /long)
		;SubZone_OS(0)	= 0

		;for i=1,nzonet do begin
			;SubZone_OS(i)	= SubZone_OS(i-1) + 1
		;endfor

		; automatically generate SubZone_OS and NTorZones
		; see below
	endelse

; read zone resolution
	srf_radi	= make_array (nzonet, /long)
	srf_polo	= make_array (nzonet, /long)
	srf_toro	= make_array (nzonet, /long)
	zon_radi	= make_array (nzonet, /long)
	zon_polo	= make_array (nzonet, /long)
	zon_toro	= make_array (nzonet, /long)

	a	= 0
	b	= 0
	c	= 0
	for i=0,nzonet-1 do begin
		reads, scrape(lun), a, b, c
		;print, a, b, c
		srf_radi(i)	= a
		srf_polo(i)	= b
		srf_toro(i)	= c
		zon_radi(i)	= a-1
		zon_polo(i)	= b-1
		zon_toro(i)	= c-1
	endfor

; setup derived values
	phi_pl_os	= make_array (nzonet+1, /long)
	phi_pl_os(0)	= 0
	mesh_p_os	= make_array (nzonet+1, /long)
	mesh_p_os(0)	= 0
	grid_p_os	= make_array (nzonet+1, /long)
	grid_p_os(0)	= 0
	for i=1,nzonet do begin
		phi_pl_os(i)	= phi_pl_os(i-1) + srf_toro(i-1)
		mesh_p_os(i)	= mesh_p_os(i-1) + zon_toro(i-1)*zon_polo(i-1)*zon_radi(i-1)
		grid_p_os(i)	= grid_p_os(i-1) + srf_toro(i-1)*srf_polo(i-1)*srf_radi(i-1)
	endfor


; setup radial and poloidal plasma range (r_surf_pl_trans_range, p_surf_pl_trans_range)
	r_surf_pl_trans_range	= make_array (2, nzonet, /long)
	for iz=0,nzonet-1 do begin
		r_surf_pl_trans_range(0,iz)	= srf_radi(iz)-1
		r_surf_pl_trans_range(1,iz)	= 0
	endfor
	p_surf_pl_trans_range	= make_array (2, nzonet, /long)
	for iz=0,nzonet-1 do begin
		p_surf_pl_trans_range(0,iz)	= srf_polo(iz)-1
		p_surf_pl_trans_range(1,iz)	= 0
	endfor


; non default surfaces
	print, 'non default surfaces:'

	; radial
	reads, scrape(lun), a
	print, '  radial: ', a
	for i=1,a do begin
		;reads, scrape(lun), str
		;reads, scrape(lun), str
		reads, scrape(lun), b, iz, c
		reads, scrape(lun), str
		r_surf_pl_trans_range(0,iz)	= min([r_surf_pl_trans_range(0,iz), b])
		r_surf_pl_trans_range(1,iz)	= max([r_surf_pl_trans_range(1,iz), b])
	endfor

	; poloidal
	reads, scrape(lun), a
	print, 'poloidal: ', a
	for i=1,a do begin
		reads, scrape(lun), b, iz, c
		reads, scrape(lun), str
		p_surf_pl_trans_range(0,iz)	= min([p_surf_pl_trans_range(0,iz), b])
		p_surf_pl_trans_range(1,iz)	= max([p_surf_pl_trans_range(1,iz), b])
	endfor

	; toroidal
	reads, scrape(lun), a
	print, 'toroidal: ', a
	for i=1,a do begin
		reads, scrape(lun), str
		reads, scrape(lun), str
	endfor

; non transparent surfaces
	print, 'non transparent surfaces:'

	; radial
	reads, scrape(lun), a
	print, '  radial: ', a
	for i=1,a do begin
		reads, scrape(lun), b, iz, c
		reads, scrape(lun), str
		r_surf_pl_trans_range(0,iz)	= min([r_surf_pl_trans_range(0,iz), b])
		r_surf_pl_trans_range(1,iz)	= max([r_surf_pl_trans_range(1,iz), b])
	endfor

	; poloidal
	reads, scrape(lun), a
	print, '  poloidal: ', a
	for i=1,a do begin
		reads, scrape(lun), b, iz, c
		reads, scrape(lun), str
		p_surf_pl_trans_range(0,iz)	= min([p_surf_pl_trans_range(0,iz), b])
		p_surf_pl_trans_range(1,iz)	= max([p_surf_pl_trans_range(1,iz), b])
	endfor


	if (v1 ge 1) then begin
	;for iz=0,nzonet-1 do begin
		;r_surf_pl_trans_range(1,iz)	= srf_radi(iz)-1
		;r_surf_pl_trans_range(0,iz)	= 0
	;endfor

	; DIII-D grids
	for i=0,nzonet/3-1 do begin
	r_surf_pl_trans_range(0,0+3*i)	= 1
	r_surf_pl_trans_range(1,0+3*i)	= srf_radi(0)-1
	r_surf_pl_trans_range(0,1+3*i)	= 1
	r_surf_pl_trans_range(1,1+3*i)	= srf_radi(1)-1
	r_surf_pl_trans_range(0,2+3*i)	= 0
	r_surf_pl_trans_range(1,2+3*i)	= srf_radi(2)-2
	endfor

	; disconnected double null grids
;	for i=0,nzonet/6-1 do begin
;	r_surf_pl_trans_range(0,0+6*i)	= 1
;	r_surf_pl_trans_range(1,0+6*i)	= srf_radi(0)-1
;	r_surf_pl_trans_range(0,1+6*i)	= 0
;	r_surf_pl_trans_range(1,1+6*i)	= srf_radi(1)-1
;	r_surf_pl_trans_range(0,2+6*i)	= 0
;	r_surf_pl_trans_range(1,2+6*i)	= srf_radi(2)-2
;	r_surf_pl_trans_range(0,3+6*i)	= 0
;	r_surf_pl_trans_range(1,3+6*i)	= srf_radi(3)-2
;	r_surf_pl_trans_range(0,4+6*i)	= 1
;	r_surf_pl_trans_range(1,4+6*i)	= srf_radi(4)-1
;	r_surf_pl_trans_range(0,5+6*i)	= 1
;	r_surf_pl_trans_range(1,5+6*i)	= srf_radi(5)-1
;	endfor

	; TEXTOR 4x4 grids
;	for i=0,nzonet/4-1 do begin
;	r_surf_pl_trans_range(0,0+4*i)	= 1
;	r_surf_pl_trans_range(1,0+4*i)	= srf_radi(0)-1
;	r_surf_pl_trans_range(0,1+4*i)	= 0
;	r_surf_pl_trans_range(1,1+4*i)	= srf_radi(1)-1
;	r_surf_pl_trans_range(0,2+4*i)	= 0
;	r_surf_pl_trans_range(1,2+4*i)	= srf_radi(2)-1
;	r_surf_pl_trans_range(0,3+4*i)	= 0
;	r_surf_pl_trans_range(1,3+4*i)	= srf_radi(2)-2
;	endfor
	endif

	; plasma transport range
	print, 'plasma transport range:'
	print, 'zone   r1 -> r2      p1 -> p2'
	for iz=0,nzonet-1 do begin
		print, iz, r_surf_pl_trans_range(0,iz), r_surf_pl_trans_range(1,iz), p_surf_pl_trans_range(0,iz), p_surf_pl_trans_range(1,iz)
	endfor


	free_lun, lun
	iflag	= 0
	return
end; pro read_grid_info
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; read actual grid data
pro read_grid_data, grid_dir, iflag
	common	config_cb
	common	grid_info_cb
	common	grid_data_cb, $
		phi_plane, rg, zg, psin, $
		nc_pl, ncell_n1, idcell, Rmag, Zmag

	loaded_grid	= ''
	iflag		= -1
; test if data file exists
	data_file	= grid_dir+'/'+geometry_dir+'/'+grid_data_file
	if not file_test(data_file) then begin
		print, 'grid data file not found: ', data_file
		return
	endif

; prepare data arrays
        phi_plane	= make_array (phi_pl_os(nzonet), /fl)
        rg		= make_array (grid_p_os(nzonet), /fl)
        zg		= make_array (grid_p_os(nzonet), /fl)

; open data file
	print, 'reading grid data from file: ', data_file
        openr, lun, data_file, /get_lun
	print, 'total number of zones:', nzonet
	print, 'resolution in zone:'
	ir	= 0L
	ip	= 0L
	it	= 0L
        for iz=0,nzonet-1 do begin
                readf, lun, ir, ip, it
		print, iz, ':', ir, ip, it

                tmp_R   = make_array(ir*ip)
                tmp_Z   = make_array(ir*ip)
                for k=0,it-1 do begin
                        readf, lun, tmp_phi
                        phi_plane(phi_pl_os(iz) + k)    = tmp_phi

                        readf, lun, tmp_R
                        readf, lun, tmp_Z

                        i1      = grid_p_os(iz) + k*ir*ip
                        i2      = i1 + ir*ip - 1

                        RG(i1:i2)       = tmp_R
                        ZG(i1:i2)       = tmp_Z
                endfor
        endfor
        free_lun, lun

; setup SubZoneOS and NTorZone from grid_info_cb
; at this point all subzone within a toroidal zone must have the same toroidal length (and position)
	NTorZones = 1

	; scan zones and determine number of toroidal zones
	phi_last  = phi_plane(phi_pl_os(0))
	for iz=1,nzonet-1 do begin
		phi = phi_plane(phi_pl_os(iz))

		if (phi ne phi_last) then begin
			NTorZones = NTorZones + 1
			phi_last  = phi
		endif
	endfor
	print, "found ", NTorZones, " toroidal zone(s)"

	; now setup zone index array
	SubZone_OS	= make_array (NTorZones+1, /long)
	SubZone_OS(0)	= 0
	itz		= 0
	phi_last	= phi_plane(phi_pl_os(0))
	for iz=1,nzonet-1 do begin
		phi = phi_plane(phi_pl_os(iz))

		if (phi ne phi_last) then begin
			itz = itz + 1
			SubZone_OS(itz) = iz
			phi_last  = phi
		endif
	endfor
	SubZone_OS(NTorZones) = nzonet
	print, "sub-zone offsets: ", SubZone_OS




; read physical cell data
	data_file	= grid_dir+'/'+geometry_dir+'/'+physical_cell_data_file
	if not file_test(data_file) then begin
		print, 'physical cell data file not found: ', data_file
		return
	endif

	print, 'reading physical cell data from file: ', data_file
	openr, lun, data_file, /get_lun
	readf, lun, ncell, nc_pl, ncell_n1
	if (ncell ne mesh_p_os(nzonet)) then begin
		print, 'resolution in physical cell data file does not match grid data!'
		return
	endif
	idcell		= make_array (ncell, /fl)
	readf, lun, idcell
	free_lun, lun

	loaded_grid	= grid_dir
	iflag		= 0

; test if data file for magnetic axis exists
	data_file	= grid_dir+'/'+geometry_dir+'/'+mag_axis_data_file
	if not file_test(data_file) then begin
		print, 'magnetic axis data file not found: ', data_file
		;Rmag	= 175.0
		Rmag    = total(RG) / grid_p_os(nzonet)
		Zmag	=   0.0
		print, 'using Rmag = ',Rmag,', Zmag = ',Zmag
		return
	endif
        openr, lun, data_file, /get_lun
	readf, lun, Rmag, Zmag
	free_lun, lun

	return

end; pro read_grid_data
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro read_psin_data, grid_dir, iflag
	common	config_cb
	common	grid_info_cb
	common	grid_data_cb

	loaded_psi_grid	= ''
	iflag		= -1
        psin		= make_array (grid_p_os(nzonet), /fl)
; test if data file exists
	data_file	= grid_dir+'/'+geometry_dir+'/'+psiN_file
	if not file_test(data_file) then begin
		print, 'psin data file not found: ', data_file
		return
	endif

; open data file
	print, 'reading psin data from file: ', data_file
        openr, lun, data_file, /get_lun
	readf, lun, psin
	free_lun, lun

	loaded_psi_grid	= grid_dir
	iflag		= 0
	return
end; pro read_psin_data
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; load emc3 grid from directory grid_dir
pro load_grid, grid_dir
	common	grid_info_cb
	common	config_cb

	;grid_dir	= sim_dir+'/'+get_selected_machine()+'/'+get_selected_shot()
	if (grid_dir eq loaded_grid) then return

	read_grid_info, grid_dir, iflag
	if (iflag eq 0) then read_grid_data, grid_dir, iflag
	if (iflag eq 0) then read_psin_data, grid_dir, iflag
end; pro read_grid
;-----------------------------------------------------------------------



;pro plot_emc3_grid, itz, it
	;common  grid_info_cb
	;common  grid_data_cb
;
	;xmin	= min(RG)
	;xmax	= max(RG)
	;ymin	= min(ZG)
	;ymax	= max(ZG)
	;plot, [xmin, xmax], [ymin, ymax], xstyle=1, ystyle=1, /nodata
;
	;xarr	= make_array (5, /fl)
	;yarr	= make_array (5, /fl)
;
	;for iz=SubZone_OS(itz),SubZone_OS(itz+1)-1 do begin
	;for i=0,SRF_RADI(iz)-2 do begin
	;for j=0,SRF_POLO(iz)-2 do begin
		;ig	= i + (j+it*(SRF_POLO(iz)-1))*(SRF_RADI(iz)-1)+MESH_P_OS(iz)
		;;if (ID_TEM(ig) eq 1) then continue
		;i1	= i + (j+it*SRF_POLO(iz))*SRF_RADI(iz)+GRID_P_OS(iz)
		;i2	= i1 + SRF_RADI(iz)
		;i3	= i2 + 1
		;i4	= i1 + 1
;
		;xarr(0)	= RG(i1)
		;xarr(1)	= RG(i2)
		;xarr(2)	= RG(i3)
		;xarr(3)	= RG(i4)
		;xarr(4)	= RG(i1)
;
		;yarr(0)	= ZG(i1)
		;yarr(1)	= ZG(i2)
		;yarr(2)	= ZG(i3)
		;yarr(3)	= ZG(i4)
		;yarr(4)	= ZG(i1)
;
		;oplot, xarr, yarr
	;endfor
	;endfor
	;endfor
;end; pro plot_emc3_grid
;;-----------------------------------------------------------------------
;pro plot_grid, phi
	;plot_emc3_grid, 0, 8
;end; pro plot_grid
;;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro init_grid_module
	common	grid_info_cb

	loaded_grid	= ''
end; pro init_grid_module
;-----------------------------------------------------------------------
