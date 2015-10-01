;-----------------------------------------------------------------------
; module:	iso_elements
; date:		Aug 16th, 2007
;
; author:	Heinke Frerichs
; email:	hfrerichs@wisc.edu
;
; description:	Isoparametric Elements
;
; contains:	cell_to_corner
;		xy_to_rs
;		rs_to_xy
;		find_cell
;		femint
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro cell_to_corner, physQ, physQ_corner
	common	grid_info_cb
	common	grid_data_cb

	print, 'mapping plasma data to grid points ...'
	physQ_corner	= make_array(grid_p_os(nzonet))
	wcorner		= make_array(grid_p_os(nzonet))
	jc		= make_array(8, /long)
	wc		= make_array(8, /fl)

	physQ_corner(*)	= 0.0
	wcorner(*)	= 0.0
	wc(*)		= 1.0

	for itz=0,ntorzones-1 do begin
	for iz=subzone_os(itz),subzone_os(itz+1)-1 do begin
	for it=0,zon_toro(iz)-1 do begin
	for ip=0,zon_polo(iz)-1 do begin
	for ir=0,zon_radi(iz)-1 do begin
		ic	= ir + (ip + it*zon_polo(iz))*zon_radi(iz) + mesh_p_os(iz)
		id	= idcell(ic) - 1
		if (id lt nc_pl and id ge 0) then begin
			jc(0)   =  ir + (ip + it*srf_polo(iz))*srf_radi(iz)  +  grid_p_os(iz)
			jc(1)   =  jc(0)  +  1
			jc(3)   =  jc(0)  +  srf_radi(iz)
			jc(2)   =  jc(3)  +  1
			jc(4:7) =  jc(0:3)  +  srf_polo(iz) * srf_radi(iz)

			;wc(*)	= 1.0
			wcorner(jc)		= wcorner(jc) + wc
			physQ_corner(jc)	= physQ_corner(jc) + wc * physQ(id)
		endif
	endfor
	endfor
	endfor
	endfor
	endfor

	for ic=0L,grid_p_os(nzonet)-1 do begin
		if (wcorner(ic) ne 0.0) then begin
			physQ_corner(ic)	= physQ_corner(ic) / wcorner(ic)
		endif
	endfor

	print, '... done'
end; cell_to_corner
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro xy_to_rs, x, y, xp, yp, r, s
	; x, y: node-cooridnates of quadrilateral
	r = -2.0
	s = -2.0

	xb  = x(0) - x(1) + x(2) - x(3)
	yb  = y(0) - y(1) + y(2) - y(3)

	xcx = x(0) + x(1) - x(2) - x(3)
	ycx = y(0) + y(1) - y(2) - y(3)

	xce = x(0) - x(1) - x(2) + x(3)
	yce = y(0) - y(1) - y(2) + y(3)

	a   = 0.5 * ((x(2)-x(0))*(y(3)-y(1)) - (x(3)-x(1))*(y(2)-y(0)))

	j1  = (x(2)-x(3))*(y(0)-y(1)) - (x(0)-x(1))*(y(2)-y(3))
	j2  = (x(1)-x(2))*(y(0)-y(3)) - (x(0)-x(3))*(y(1)-y(2))

	x0  = 0.25 * (x(0)+x(1)+x(2)+x(3))
	y0  = 0.25 * (y(0)+y(1)+y(2)+y(3))

	xp0 = xp - x0
	yp0 = yp - y0

	b_xi  =  a - xp0*yb + yp0*xb
	b_eta = -a - xp0*yb + yp0*xb

	c_xi  = xp0*ycx - yp0*xcx
	c_eta = xp0*yce - yp0*xce

	b2_jcxi	= b_xi*b_xi - 2.0*j1*c_xi
	if (b2_jcxi lt 0.0) then return

	xip1	= -sqrt(b2_jcxi) - b_xi
	if (xip1 eq 0.0) then return
	xip	= 2.0*c_xi / xip1
	r	= xip

	b2_jceta= b_eta*b_eta + 2.0*j2*c_eta
	if (b2_jceta lt 0.0) then return

	etap1	=  sqrt(b2_jceta) - b_eta
	if (etap1 eq 0.0) then return
	etap	= 2.0*c_eta / etap1
	s	= etap
end; xy_to_rs
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro rs_to_xy, rg, zg, r, s, xp, yp
	ar  =	0.25 * (rg(0) + rg(3) + rg(2) + rg(1))
	br  =	0.25 * (rg(2) + rg(1) - rg(0) - rg(3))
	cr  =	0.25 * (rg(2) + rg(3) - rg(0) - rg(1))
	dr  =	0.25 * (rg(0) + rg(2) - rg(3) - rg(1))

	az  =	0.25 * (zg(0) + zg(3) + zg(2) + zg(1))
	bz  =	0.25 * (zg(2) + zg(1) - zg(0) - zg(3))
	cz  =	0.25 * (zg(2) + zg(3) - zg(0) - zg(1))
	dz  =	0.25 * (zg(0) + zg(2) - zg(3) - zg(1))

	xp  =	ar + r * br + s * cr + r * s * dr
	yp  =	az + r * bz + s * cz + r * s * dz
end; rs_to_xy
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; find cell at toroidal location itz, it, t for coordinates Rin, Zin
pro find_cell, Rin, Zin, itz, iz, it, ip, ir, id, r, s, t, ierr
	common	grid_info_cb
	common	grid_data_cb

	ierr	= 1

	s=0.0
	r=0.0
	id	= make_array(8, /long)
	rnode	= make_array(4, /fl)
	znode	= make_array(4, /fl)
; find subdomain, radial and poloidal index
	iz	= SubZone_OS(itz)
	ip	= 0
	ir	= 0
	for iz=SubZone_OS(itz),SubZone_OS(itz+1)-1 do begin
	for ip=0,zon_polo(iz)-1 do begin
	for ir=0,zon_radi(iz)-1 do begin
		id(0)	= ir  +  (ip + it * srf_polo(iz)) * srf_radi(iz)  +  grid_p_os(iz)                                     
		id(1)	= id(0) + 1
		id(3)	= id(0) + srf_radi(iz)
		id(2)	= id(3) + 1

		id(4:7)	= id(0:3) + srf_polo(iz) * srf_radi(iz)
		rnode	= t * rg(id(4:7)) + (1-t) * rg(id(0:3))
		znode	= t * zg(id(4:7)) + (1-t) * zg(id(0:3))

		xy_to_rs, rnode, znode, Rin, Zin, r, s

		if (abs(r) le 1.0  and  abs(s) le 1.0) then begin
			;print, iz, it, ip, ir, r, s
			ierr	= 0
			return
		endif
	endfor
	endfor
	endfor

end; find_cell
;.......................................................................
pro smart_find_cell, Rin, Zin, itz, iz, it, ip, ir, id, r, s, t, iz_start, ip_start, ir_start, ierr
end; smart_find_cell
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
function femint, fn, r, s, t
	c_r	= [-1.0,  1.0,  1.0, -1.0, -1.0,  1.0,  1.0, -1.0]
	c_s	= [-1.0, -1.0,  1.0,  1.0, -1.0, -1.0,  1.0,  1.0]
	c_t	= [ 0.0,  0.0,  0.0,  0.0,  1.0,  1.0,  1.0,  1.0]

	res	= total(fn * 0.1250 * (1.0 + c_r*r) * (1.0 + c_s*s) * (1.0 + (2*c_t - 1.0)*(2*t - 1.0)))

	return, res
end; femint
;-----------------------------------------------------------------------
