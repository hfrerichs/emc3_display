;-----------------------------------------------------------------------
; module:	poloidal_cut
; date:		Jul 12th, 2007
;
; author:	Heinke Frerichs
; email:	h.frerichs@fz-juelich.de
;
; description:	provides routines to plot poloidal cuts
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; store toroidal zone itz and toroidal cell index it in pc_itz and pc_it
pro poloidal_cut, phi, cut_grid, iflag
	common	grid_info_cb
	common	grid_data_cb

	iflag	= -1
	pc_itz	= -1
	pc_it	=  0
	t	=  0.0
	for itz=0,NTorZones-1 do begin
	for iz=SubZone_OS(itz),SubZone_OS(itz+1)-1 do begin
	for it=0,srf_toro(iz)-2 do begin
		phi1	= phi_plane(phi_pl_os(iz)+it  )
		phi2	= phi_plane(phi_pl_os(iz)+it+1)
		if ((phi1 le phi) and (phi2 ge phi)) then begin
			pc_itz	= itz
			pc_it	= it
			t	= (phi - phi2) / (phi1 - phi2)
		endif
	endfor
	endfor
	endfor
	if (pc_itz eq -1) then return

	; get cut resolution
	n_cut_subzones	= SubZone_OS(pc_itz+1) - SubZone_OS(pc_itz)
	n_cut_os	= make_array(n_cut_subzones + 1, /long)
	n_cut_os(0)	= 0L
	for iz=SubZone_OS(pc_itz),SubZone_OS(pc_itz+1)-1 do begin
		i		= iz - SubZone_OS(pc_itz)
		n_cut_os(i+1)	= n_cut_os(i)  +  srf_polo(iz) * srf_radi(iz)
	endfor


	; prepare arrays with grid points for plotting
	rg_cut	= make_array(n_cut_os(n_cut_subzones), /fl)
	zg_cut	= make_array(n_cut_os(n_cut_subzones), /fl)
	thetag_cut	= make_array(n_cut_os(n_cut_subzones), /fl)
	rming_cut	= make_array(n_cut_os(n_cut_subzones), /fl)
	psig_cut	= make_array(n_cut_os(n_cut_subzones), /fl)


	for iz=SubZone_OS(pc_itz),SubZone_OS(pc_itz+1)-1 do begin
		n	= srf_polo(iz) * srf_radi(iz)
		for j=0,srf_polo(iz)-1 do begin
		for i=0,srf_radi(iz)-1 do begin
			ig      = i + (j+pc_it*srf_polo(iz))*srf_radi(iz)  +  grid_p_os(iz)
			ic	= i + j*srf_radi(iz)  +  n_cut_os(iz - SubZone_OS(pc_itz))

			rg_cut(ic)	= t * rg(ig) + (1-t) * rg(ig+n)
			zg_cut(ic)	= t * zg(ig) + (1-t) * zg(ig+n)
			psig_cut(ic)	= t * psin(ig) + (1-t) * psin(ig+n)
		endfor
		endfor
	endfor

	; theta, rmin and psi coordinates for plotting
	thetag_cut	= atan(zg_cut - Zmag, rg_cut - Rmag) * 180 / !pi
	for i=0L,n_cut_os(n_cut_subzones)-1 do if (thetag_cut(i) lt 0.0) then thetag_cut(i) = thetag_cut(i) + 360.0
	rming_cut	= sqrt((rg_cut - Rmag)^2 + (zg_cut - Zmag)^2)

	cut_grid	= create_struct('itz',pc_itz,'it',pc_it,'t',1-t,'n_tot',n_cut_os(n_cut_subzones),'os',n_cut_os, $
				'rg',rg_cut,'zg',zg_cut,'thetag',thetag_cut,'rming',rming_cut,'psig',psig_cut)

	iflag	= 0
	return
end; pro poloidal_cut
;-----------------------------------------------------------------------
