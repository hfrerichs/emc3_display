;-----------------------------------------------------------------------
; module:	target
; date:		Aug 20th, 2007
;
; author:	Heinke Frerichs
; email:	h.frerichs@fz-juelich.de
;
; description:	plotting deposition data routines
;
; contains:	load_target_geo
;-----------------------------------------------------------------------
;@PiP



pro ltg_read, lun, data
        dummy   = ''
        readf, lun ,dummy
        print, dummy
        readf, lun, data
end; ltd_read

pro ldd_read, lun, data, text
        print, text
        readf, lun, data
end; ldd_read



;=======================================================================
pro load_target_info, base_dir, labels
	common	config_cb

        ; target geometry
        common  ltg_common, NLIM, LIM_POSI_A, LIM_POSI_B, LIM_NPOL, LIM_NTOR, LIM_LABEL, $
                LIM_R1, LIM_R2, LIM_R3, LIM_R4, LIM_Z1, LIM_Z2, LIM_Z3, LIM_Z4,$
                LIM_L1, LIM_L2, LIM_L3, LIM_L4, $
                LIM_PHI1, LIM_PHI2, LIM_AREA

	; target info file
	element_info_file	= base_dir+'/'+depo_file(0);TARGET_ELEMENT_INFO'

	
        NLIM    = 0
        if (not file_test(element_info_file)) then begin
                print, "couldn't read geometry of targets, deposition data won't be available"
                return
        endif


        ; open target information file
        dummy   = ''
        openr, lun, element_info_file, /get_lun
        readf, lun, FORMAT='(I8,a)', NLIM, dummy

        print, 'number of limiters: ', NLIM
        LIM_POSI_A      = make_array (NLIM, /long)
        LIM_POSI_B      = make_array (NLIM, /long)
        LIM_NPOL        = make_array (NLIM, /long)
        LIM_NTOR        = make_array (NLIM, /long)
	LIM_LABEL	= make_array (NLIM, /string)
        I1      = 0L
        I2      = 0L

        ; read target information for each limiter
        for i=0, NLIM-1 do begin
		readf, lun, dummy
		print, dummy
		LIM_LABEL[i]	= dummy
                readf, lun, FORMAT='(I8,I8,a)', I1, I2, dummy
                LIM_NPOL[i]     = I1
                LIM_NTOR[i]     = I2
                print, FORMAT='("Limiter ", I3, ": poloidal and toroidal resolution: ", I6, I6)',$
                        i, LIM_NPOL[i], LIM_NTOR[i]
                readf, lun, FORMAT='(I8,I8,a)', I1, I2, dummy
                LIM_POSI_A[i]   = I1
                LIM_POSI_B[i]   = I2
                print, FORMAT='("Limiter ", I3, ": starting and ending index:        ", I6,I6)',$
                        i, LIM_POSI_A[i], LIM_POSI_B[i]

                if ((LIM_NPOL[i]-1)*(LIM_NTOR[i]-1) ne LIM_POSI_B[i]-LIM_POSI_A[i]+1) then begin
                        print,"resolution of limiter does not matche element number! Stopping"
                        stop
                endif
        endfor
        free_lun, lun

	labels	= LIM_LABEL
end; load_target_info
;=======================================================================



;=======================================================================
pro load_target_geo, base_dir
	common	config_cb
	common	ltg_common

        LIM_R1  = DBLARR(LIM_POSI_B[NLIM-1])
        LIM_R2  = LIM_R1
        LIM_R3  = LIM_R1
        LIM_R4  = LIM_R1
        LIM_Z1  = LIM_R1
        LIM_Z2  = LIM_R1
        LIM_Z3  = LIM_R1
        LIM_Z4  = LIM_R1
        LIM_L1  = LIM_R1
        LIM_L2  = LIM_R1
        LIM_L3  = LIM_R1
        LIM_L4  = LIM_R1
        LIM_PHI1        = LIM_R1
        LIM_PHI2        = LIM_R1
        LIM_AREA        = LIM_R1


        ; read target geometry
	geo_info_file		= base_dir+'/'+depo_file(1);TARGET_GEO_INFO'
	dummy			= ''
        print, "Reading: "+geo_info_file
        openr, lun, geo_info_file, /get_lun

        readf, lun, FORMAT='(I8,a)', I1, dummy
        if (I1 ne LIM_POSI_B[NLIM-1]) then begin
                print, "Number of limiter elemtes are not equal: ", LIM_POSI_B[NLIM-1], I1
                stop
        endif

        ;print,I1,dummy
        ltg_read, lun, LIM_R1
        ltg_read, lun, LIM_R2
        ltg_read, lun, LIM_R3
        ltg_read, lun, LIM_R4
        ltg_read, lun, LIM_Z1
        ltg_read, lun, LIM_Z2
        ltg_read, lun, LIM_Z3
        ltg_read, lun, LIM_Z4
        ltg_read, lun, LIM_L1
        ltg_read, lun, LIM_L2
        ltg_read, lun, LIM_L3
        ltg_read, lun, LIM_L4
        ltg_read, lun, LIM_PHI1
        ltg_read, lun, LIM_PHI2
        ltg_read, lun, LIM_AREA
        print,"... done!"

        free_lun, lun
end; load_target_geo
;=======================================================================



;=======================================================================
pro load_target_data, base_dir
	common	config_cb
	common	ltg_common

        ; limiter subresolution
        common  ltd_common,     LIM_SUBRES_RCOR, LIM_SUBRES_ZCOR, LIM_SUBRES_PCOR,$
                                LIM_SUBRES_LCOR, LIM_SUBRES_NPOL, LIM_SUBRES_NTOR, $
                                LIM_SUBRES_DEPO_P,  LIM_SUBRES_DEPO_E,  LIM_SUBRES_DEPO_DE,$
                                LIM_SUBRES_DEPO_TE, LIM_SUBRES_DEPO_TI, LIM_SUBRES_DEPO_CL,$
                                LIM_SUBRES_DEPO_AV,$
                                LIM_NION,           LIM_SUBRES_DEPO_Nimp

	data_file	= base_dir+'/'+depo_file(2);TARGET_INFO'

        LIM_SUBRES_NPOL = 0
        LIM_SUBRES_NTOR = 0
        I1      = 0L

        print, FORMAT='("Loading deposition data from ",A," ...")', data_file
        ;openr, lun, r_select+'/'+DEPOSITION_DATA, /get_lun
        openr, lun, data_file, /get_lun
        readf, lun, FORMAT='(I6,I6,I6)', LIM_SUBRES_NPOL, LIM_SUBRES_NTOR, I1
        ;readf, lun, FORMAT='(I6)', LIM_NION
        print, FORMAT='("Subresolution of limiter elements NPOL, NTOR: ", I8, I8)', LIM_SUBRES_NPOL, LIM_SUBRES_NTOR

        if (I1 ne LIM_POSI_B[NLIM-1]) then begin
                print, "Number of limiter elements does not match", I1, LIM_POSI_B[NLIM-1]
                stop
        endif

        LIM_SUBRES_RCOR         = make_array (2, 2, LIM_POSI_B[NLIM-1])
        LIM_SUBRES_ZCOR         = make_array (2, 2, LIM_POSI_B[NLIM-1])
        LIM_SUBRES_PCOR         = make_array (2, 2, LIM_POSI_B[NLIM-1])
        LIM_SUBRES_LCOR         = make_array (2, 2, LIM_POSI_B[NLIM-1])
        LIM_SUBRES_DEPO_P       = make_array (LIM_SUBRES_NPOL,   LIM_SUBRES_NTOR,   LIM_POSI_B[NLIM-1])
        LIM_SUBRES_DEPO_E       = make_array (LIM_SUBRES_NPOL,   LIM_SUBRES_NTOR,   LIM_POSI_B[NLIM-1])
        LIM_SUBRES_DEPO_DE      = make_array (LIM_SUBRES_NPOL,   LIM_SUBRES_NTOR,   LIM_POSI_B[NLIM-1])
        LIM_SUBRES_DEPO_TE      = make_array (LIM_SUBRES_NPOL,   LIM_SUBRES_NTOR,   LIM_POSI_B[NLIM-1])
        LIM_SUBRES_DEPO_TI      = make_array (LIM_SUBRES_NPOL,   LIM_SUBRES_NTOR,   LIM_POSI_B[NLIM-1])
        LIM_SUBRES_DEPO_CL      = make_array (LIM_SUBRES_NPOL,   LIM_SUBRES_NTOR,   LIM_POSI_B[NLIM-1])
        LIM_SUBRES_DEPO_AV      = make_array (LIM_SUBRES_NPOL,   LIM_SUBRES_NTOR,   LIM_POSI_B[NLIM-1])

        ldd_read, lun, LIM_SUBRES_RCOR, "loading R-Coordinates..."
        ldd_read, lun, LIM_SUBRES_ZCOR, "loading Z-Coordinates..."
        ldd_read, lun, LIM_SUBRES_LCOR, "loading L-Coordinates..."
	;LIM_SUBRES_LCOR	= - LIM_SUBRES_LCOR
	;LIM_SUBRES_LCOR	= LIM_SUBRES_LCOR - 1.8
;	LIM_SUBRES_LCOR	= 360.0 - LIM_SUBRES_LCOR
        ldd_read, lun, LIM_SUBRES_PCOR, "loading PHI-Coordinates..."
        ldd_read, lun, LIM_SUBRES_DEPO_P, "loading DEPO_P..."
        ldd_read, lun, LIM_SUBRES_DEPO_E, "loading DEPO_E..."
        ldd_read, lun, LIM_SUBRES_DEPO_DE, "loading DEPO_DE..."
        ldd_read, lun, LIM_SUBRES_DEPO_TE, "loading DEPO_TE..."
        ldd_read, lun, LIM_SUBRES_DEPO_TI, "loading DEPO_TI..."
        ldd_read, lun, LIM_SUBRES_DEPO_CL, "loading DEPO_CL..."
        ldd_read, lun, LIM_SUBRES_DEPO_AV, "loading DEPO_AV..."
        free_lun, lun
        print,"... done!"

	;for i=0,LIM_SUBRES_NPOL do begin
	;for j=0,LIM_SUBRES_NTOR do begin
	;for k=0,LIM_POSI_B[NLIM-1]-1 do begin

	;endfor
	;endfor
	;endfor
end; load_target_data
;=======================================================================



;=======================================================================
pro plot_target_data, data_id, limiter=limiter
	common	ltg_common
	common	ltd_common
	common	plot_cb
	common	config_cb


	n_levels	= 250
	title_str	= get_selected_shot() + ' ' + get_selected_case()
	print, 'plotting data...'

	; open plot window
	if (check_button('save_output')) then begin
	; for eps output
		data_file	= get_entry('output_file')
		file_type	= 'eps'

		set_plot, 'PS'
		device, file=data_file, /ENCAPSUL, /COLOR, BITS=8, FONT_SIZE=9, xsize=12, ysize=08

		char_size	= 1.4
		char_thick	= 3.0
		xl_margin	=   7
		xr_margin	=  10
		xl_margin_cb	=  49
		xr_margin_cb	=   2
	endif else begin
	; for output on screen
		file_type	= 'X'
		set_plot, 'X'
		device, retain=2, decomposed=0
		xsize	= 800	
		ysize	= 600
		base		= widget_base(/row, xsize=xsize, /base_align_center, title=title_str, uname='plot_window')
		plot_window	= widget_draw(base,xsize=xsize ,ysize=ysize,/button_events)
		widget_control, base, /realize

		char_size	= 1.2
		char_thick	= 1.5
		xl_margin	=   7
		xr_margin	=  16
		xl_margin_cb	= 106
		xr_margin_cb	=   2
	endelse


	; select limiter to plot
	if (keyword_set(limiter)) then begin
		LIMI	= limiter
	endif else begin
		LIMI	= 0
	endelse


	; plotting range
	phi_start	= LIM_SUBRES_PCOR[0,              0,LIM_POSI_A[LIMI]-1]*180.0/!PI
	phi_end		= LIM_SUBRES_PCOR[0,1,LIM_POSI_B[LIMI]-1]*180.0/!PI
	;phi_end		= LIM_SUBRES_PCOR[0,LIM_SUBRES_NTOR,LIM_POSI_B[LIMI]-1]*180.0/!PI
	Z_start		= min(LIM_SUBRES_LCOR[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
	Z_end		= max(LIM_SUBRES_LCOR[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])

	; data range
	case data_id of
	0: begin
		maxVal = MAX(LIM_SUBRES_DEPO_P[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])*10
		minVal = MIN(LIM_SUBRES_DEPO_P[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])*10
        end
        1: begin
		maxVal = MAX(LIM_SUBRES_DEPO_E[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])/100.0
		minVal = MIN(LIM_SUBRES_DEPO_E[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])/100.0
        end
        2: begin
		maxVal = MAX(LIM_SUBRES_DEPO_DE[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
		minVal = MIN(LIM_SUBRES_DEPO_DE[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
        end
        3: begin
		maxVal = MAX(LIM_SUBRES_DEPO_TE[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
		minVal = MIN(LIM_SUBRES_DEPO_TE[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
        end
        4: begin
		maxVal = MAX(LIM_SUBRES_DEPO_TI[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
		minVal = MIN(LIM_SUBRES_DEPO_TI[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
        end
        5: begin
		maxVal = MAX(LIM_SUBRES_DEPO_CL[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
		minVal = MIN(LIM_SUBRES_DEPO_CL[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
        end
        6: begin
		maxVal = MAX(LIM_SUBRES_DEPO_AV[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
		minVal = MIN(LIM_SUBRES_DEPO_AV[*,*,LIM_POSI_A[LIMI]-1:LIM_POSI_B[LIMI]-1])
	end
	endcase
	; data range
	str	= (get_entry('dmin'))[0]
	if str ne '' then minVal	= double(str)
	str	= (get_entry('dmax'))[0]
	if str ne '' then maxVal	= double(str)
	print, 'minVal = ', minVal, ', maxVal = ', maxVal


	; prepare plotting area
	title	= depo_arr(0,data_id)
	xtitle	= 'Toroidal Angle [deg]'
	;ytitle	= 'Z on limiting wall [cm]'
	;ytitle	= 'Poloidal Angle [deg]'
	;ytitle	= 'Length along wall [cm]'
	;ytitle	= 'Distance from Strike Point [cm]'
	ytitle	= 'Distance from upt. separatrix [cm]'
	plot, [phi_start, phi_end], [Z_start, Z_end], xstyle=5, ystyle=5, /nodata, charsize=char_size, charthick=char_thick, $
		xmargin=[xl_margin, xr_margin]


	; select profiles
	xprof	= -1000
	str	= get_entry('target_xid')
	if str ne '' then begin
		xprof	= double(str)
		openw, lunX, 'target_xprof_y'+strcompress(str, /remove_all)+'_'+depo_arr(1,data_id)+'_'+strcompress (title_str, /remove_all)+'.txt', /get_lun
	endif

	yprof	= -1000
	str	= get_entry('target_yid')
	if str ne '' then begin
		yprof	= double(str)
		openw, lunY, 'target_yprof_x'+strcompress(str, /remove_all)+'_'+depo_arr(1,data_id)+'_'+strcompress (title_str, /remove_all)+'.txt', /get_lun
	endif

	;openw, lunXY, 'target.dat', /get_lun

	larr	= make_array(4, /fl)
	parr	= make_array(4, /fl)
	Rarr	= make_array(4, /fl)
	Zarr	= make_array(4, /fl)
	nd	= (LIM_NTOR[LIMI]-1)*(LIM_NPOL[LIMI]-1)*LIM_SUBRES_NTOR*LIM_SUBRES_NPOL
	xm	= make_array(nd, /fl)
	ym	= make_array(nd, /fl)
	avx	= make_array((LIM_NPOL[LIMI]-1)*LIM_SUBRES_NPOL, 2, /fl)
	avx(*)	= 0.0
	i	= 0L
	ierr_sum	= 0
	IntV	= 0.0
	IntV1	= 0.0
	IntA	= 0.0
	; now the actual plotting is done
	for itor=0L,LIM_NTOR[LIMI]-2 do begin
	for ipol=0L,LIM_NPOL[LIMI]-2 do begin
		IR = LIM_POSI_A[LIMI] + ipol + itor*(LIM_NPOL[LIMI]-1L) -1L
		p0    = [LIM_SUBRES_PCOR[0, 0, IR]*180.0/!PI, $
		         LIM_SUBRES_PCOR[1, 0, IR]*180.0/!PI, $
		         LIM_SUBRES_PCOR[1, 1, IR]*180.0/!PI, $
		         LIM_SUBRES_PCOR[0, 1, IR]*180.0/!PI]
		L0    = [LIM_SUBRES_LCOR[0, 0, IR], $
		         LIM_SUBRES_LCOR[1, 0, IR], $
		         LIM_SUBRES_LCOR[1, 1, IR], $
		         LIM_SUBRES_LCOR[0, 1, IR]]
		R0    = [LIM_SUBRES_RCOR[0, 0, IR], $
		         LIM_SUBRES_RCOR[1, 0, IR], $
		         LIM_SUBRES_RCOR[1, 1, IR], $
		         LIM_SUBRES_RCOR[0, 1, IR]]
		Z0    = [LIM_SUBRES_ZCOR[0, 0, IR], $
		         LIM_SUBRES_ZCOR[1, 0, IR], $
		         LIM_SUBRES_ZCOR[1, 1, IR], $
		         LIM_SUBRES_ZCOR[0, 1, IR]]

		for subitor=0L,LIM_SUBRES_NTOR-1L do begin
		for subipol=0L,LIM_SUBRES_NPOL-1L do begin
			ValX = [LIM_SUBRES_DEPO_P[subipol,subitor,IR], $
				LIM_SUBRES_DEPO_E[subipol,subitor,IR], $
                        	LIM_SUBRES_DEPO_DE[subipol,subitor,IR], $
                        	LIM_SUBRES_DEPO_TE[subipol,subitor,IR], $
                        	LIM_SUBRES_DEPO_TI[subipol,subitor,IR], $
                        	LIM_SUBRES_DEPO_CL[subipol,subitor,IR], $
                        	LIM_SUBRES_DEPO_AV[subipol,subitor,IR]]

			case data_id of
			0: Val = LIM_SUBRES_DEPO_P[subipol,subitor,IR]*10
			1: Val = LIM_SUBRES_DEPO_E[subipol,subitor,IR]/100.0
                        2: Val = LIM_SUBRES_DEPO_DE[subipol,subitor,IR]
                        3: Val = LIM_SUBRES_DEPO_TE[subipol,subitor,IR]
                        4: Val = LIM_SUBRES_DEPO_TI[subipol,subitor,IR]
                        5: Val = LIM_SUBRES_DEPO_CL[subipol,subitor,IR]
                        6: Val = LIM_SUBRES_DEPO_AV[subipol,subitor,IR]
			endcase

			; 2014-09-12: trying to plot deposition data with sub-resolution,
			; but coordinates are given only for main elements now (was it always this way?)
			;phi1 = LIM_SUBRES_PCOR[subipol, subitor, IR]*180.0/!PI
			;phi2 = LIM_SUBRES_PCOR[subipol+1, subitor, IR]*180.0/!PI
			;phi3 = LIM_SUBRES_PCOR[subipol+1, subitor+1, IR]*180.0/!PI
			;phi4 = LIM_SUBRES_PCOR[subipol, subitor+1, IR]*180.0/!PI
			;L1 = LIM_SUBRES_LCOR[subipol, subitor, IR]
			;L2 = LIM_SUBRES_LCOR[subipol+1, subitor, IR]
			;L3 = LIM_SUBRES_LCOR[subipol+1, subitor+1, IR]
			;L4 = LIM_SUBRES_LCOR[subipol, subitor+1, IR]

			; calculate surface integral ----------------------------------------
			;R1 = LIM_SUBRES_RCOR[subipol, subitor, IR]
			;R2 = LIM_SUBRES_RCOR[subipol+1, subitor, IR]
			;R3 = LIM_SUBRES_RCOR[subipol+1, subitor+1, IR]
			;R4 = LIM_SUBRES_RCOR[subipol, subitor+1, IR]
			;Z1 = LIM_SUBRES_ZCOR[subipol, subitor, IR]
			;Z2 = LIM_SUBRES_ZCOR[subipol+1, subitor, IR]
			;Z3 = LIM_SUBRES_ZCOR[subipol+1, subitor+1, IR]
			;Z4 = LIM_SUBRES_ZCOR[subipol, subitor+1, IR]
			r = 1.0 * [subipol, subipol+1, subipol+1, subipol] / LIM_SUBRES_NPOL
			s = 1.0 * [subitor, subitor, subitor+1, subitor+1] / LIM_SUBRES_NTOR
			Rarr(*) = R0[0] + r(*)*(R0[1]-R0[0]) + s(*)*(R0[3]-R0[0]) + r(*)*s(*)*(R0[0]-R0[1]+R0[2]-R0[3])
			Zarr(*) = Z0[0] + r(*)*(Z0[1]-Z0[0]) + s(*)*(Z0[3]-Z0[0]) + r(*)*s(*)*(Z0[0]-Z0[1]+Z0[2]-Z0[3])
			parr(*) = p0[0] + r(*)*(p0[1]-p0[0]) + s(*)*(p0[3]-p0[0]) + r(*)*s(*)*(p0[0]-p0[1]+p0[2]-p0[3])
			larr(*) = L0[0] + r(*)*(L0[1]-L0[0]) + s(*)*(L0[3]-L0[0]) + r(*)*s(*)*(L0[0]-L0[1]+L0[2]-L0[3])

;			if ((phi1 ne phi2) or (phi3 ne phi4)) then begin
;				if (ierr_sum eq 0) then begin
;					print, 'cannot calculate area for surface integral! (weird phi)'
;					print, phi1, phi2, phi3, phi4
;				endif
;				;ierr_sum = 1
;			endif
;			if ((R1 ne R4) or (R2 ne R3)) then begin
;				if (ierr_sum eq 0) then begin
;					print, 'cannot calculate area for surface integral! (weird R)'
;					print, R1, R2, R3, R4
;				endif
;				;ierr_sum = 1
;			endif
;			if ((Z1 ne Z4) or (Z2 ne Z3)) then begin
;				if (ierr_sum eq 0) then begin
;					print, 'cannot calculate area for surface integral! (weird Z)'
;					print, Z1, Z2, Z3, Z4
;				endif
;				;ierr_sum = 1
;			endif

;			Rc   = 0.5 * (R1+R2)
;			l    = sqrt((R1-R2)^2 + (Z1-Z2)^2)
;			area = l * Rc * (phi3-phi1) / 180.0 * !PI
			Rc   = 0.5 * (R0[0]+R0[1])
			l    = sqrt((R0[0]-R0[1])^2 + (Z0[0]-Z0[1])^2)
			area = l * Rc * (p0[2]-p0[0]) / 180.0 * !PI
;
			IntV = IntV + Val * area
			if ((ValX[5] gt 0) and (ValX[5] le 1.0)) then begin
				IntV1 = IntV1 + Val * area
			endif else begin
				;Val =  0.0
			endelse
			IntA = IntA +       area
			; -------------------------------------------------------------------

			;parr = [phi1, phi2, phi3, phi4]
			;larr = [L1,   L2,   L3,   L4  ]
			xm(i)	= total(parr)/4
			ym(i)	= total(larr)/4
			iy = ipol*LIM_SUBRES_NPOL + subipol
			avx(iy, 0) = ym(i)
			avx(iy, 1) = avx(iy, 1) + Val / LIM_SUBRES_NTOR / (LIM_NTOR[LIMI]-1)
			i	= i+1

			znnn	= round((n_levels-1) * (Val-minVal) / (maxVal-minVal))
			if (Val ge maxVal) then znnn = n_levels
			if (Val le minVal) then znnn = 1


                ;if ((yprof gt -1000) and (phi3 gt yprof) and (phi1 le yprof)) then begin
                if ((yprof gt -1000) and (parr(2) gt yprof) and (parr(0) le yprof)) then begin
                        znnn = 255
                        ;printf, lunY, (L1+L2)/2, Val
                        printf, lunY, (L(0)+L(1))/2, Val
                endif
                ;if ((xprof gt -1000) and (L3 gt xprof) and (L1 le xprof)) then begin
                if ((xprof gt -1000) and (Larr(2) gt xprof) and (Larr(0) le xprof)) then begin
                        znnn = 255
                        ;printf, lunX, (phi1+phi2)/2, Val
                        printf, lunX, (phi(0)+phi(1))/2, Val
                endif

			polyfill,parr,larr,col=znnn,/data,noclip=0
			;oplot, parr, larr
			;printf, lunXY, 0.25*(phi1+phi2+phi3+phi4), 0.25*(Z1+Z2+Z3+Z4), Val
		endfor
		endfor
	endfor
	endfor
	print, 'integral  = ', IntV
	print, 'integral1 = ', IntV1
	print, 'total area [cm^2] = ', IntA

	; close output files for selected profiles
	if (yprof gt -1000) then free_lun, lunY
	if (xprof gt -1000) then free_lun, lunX
	;free_lun, lunXY

	overlay_data, poly_test

;	openw, lunAv, 'av_target.txt', /get_lun
;	for ipol=0L,(LIM_NPOL[LIMI]-1)*LIM_SUBRES_NPOL-1 do begin
;		printf, lunAv, avx(ipol,0), avx(ipol,1)
;	endfor
;	free_lun, lunAv

;	np	= (size(poly_test, /dimensions))[1]
;	px	= make_array(np, /fl)
;	py	= make_array(np, /fl)
;	px(*)	= poly_test(0,*)
;	py(*)	= 360 - poly_test(1,*)
;	inside_poly	= Inside (xm, ym, px, py)

	;for i=0,nd-1 do begin
		;if (inside_poly(i) eq  1) then oplot, [xm(i)], [ym(i)], color=250, psym=1
	;endfor
	i	= 0
	intf	= 0
	Atot	= 0
	; now the actual plotting is done
	for itor=0L,LIM_NTOR[LIMI]-2 do begin
	for ipol=0L,LIM_NPOL[LIMI]-2 do begin
		IR = LIM_POSI_A[LIMI] + ipol + itor*(LIM_NPOL[LIMI]-1L) -1L
;;; X3: needs load_target_geo		A  = LIM_AREA[IR] / (LIM_SUBRES_NTOR*LIM_SUBRES_NPOL)
		for subitor=0L,LIM_SUBRES_NTOR-1L do begin
		for subipol=0L,LIM_SUBRES_NPOL-1L do begin
			;if (inside_poly(i) eq 1) then begin
				;oplot, [xm(i)], [ym(i)], color=250, psym=1
			case data_id of
			0: f = LIM_SUBRES_DEPO_P[subipol,subitor,IR]
			1: f = LIM_SUBRES_DEPO_E[subipol,subitor,IR]
                        2: f = LIM_SUBRES_DEPO_DE[subipol,subitor,IR]
                        3: f = LIM_SUBRES_DEPO_TE[subipol,subitor,IR]
                        4: f = LIM_SUBRES_DEPO_TI[subipol,subitor,IR]
                        5: f = LIM_SUBRES_DEPO_CL[subipol,subitor,IR]
                        6: f = LIM_SUBRES_DEPO_AV[subipol,subitor,IR]
			endcase
;;; X3			intf = intf + f*A
			;endif
			i	= i + 1
		endfor
		endfor
;;; X3		Atot = Atot + LIM_AREA[IR]
	endfor
	endfor
;;; X3	print, 'integral = ', intf
;;; X3	print, 'total area = ', Atot


	; plot axis
	plot, [phi_start, phi_end], [Z_start, Z_end], xstyle=1, ystyle=1, /nodata, /noerase, $
		;title=title+' (scenario A)', xtitle=xtitle, ytitle=ytitle, $
		title=title, xtitle=xtitle, ytitle=ytitle, $
		charsize=char_size, charthick=char_thick, xmargin=[xl_margin, xr_margin]

	;oplot, [-22.5,157.5], [332.24, 332.24], color=255, thick=8, linestyle=2
	;oplot, [-22.5,157.5], [297.76, 297.76], color=255, thick=8, linestyle=2

	;xyouts, -10, 30, 'reduced D,!4v!3 (1/100)', charsize=1.3, charthick=4.0, color=255
;/----------COLOR BAR----------/
    
 n_levels = 254
 n_level_start = 1
    zmax  = maxVal
    zmin  = minVal
    level = (zmin+findgen(n_levels+1)/(n_levels)*(zmax-zmin))
    color = n_level_start + ROUND((1+findgen(n_levels))/(n_levels)*(n_levels-1))
    
    bar_dummy 	= [transpose(level),transpose(level)]
    xbar		= findgen(2)
    
	;unitsString = 'mA / cm!E2!N'
	unitsString = depo_arr(2,data_id)
    colBarStr = unitsString     ; colBarText[PhysQuantitySelect-1]
;if (plotLogScale EQ 1) then colBarStr = 'Log10( '+colBarStr+' )'
;if (diffType LE 0) then colBarStr = colBarStr+'  Relative Change in %'
    
    CONTOUR	,bar_dummy,xbar,level, $
                 /FILL, C_COLORS=color,LEVELS=level,YSTYLE=1,XSTYLE=4,$ 
                 YTITLE=colBarStr,/NOERASE, ticklen=0.2, $
		charsize=char_size, charthick=char_thick, xmargin=[xl_margin_cb, xr_margin_cb]

;-----------------

end; plot_target_data
;=======================================================================



;=======================================================================
pro plot_depo_data, data_id, limiter=limiter
	common	config_cb

	; load data
	machine		= get_selected_machine()
	shot		= get_selected_shot()
	casex		= get_selected_case()
	run		= get_selected_run()
	base_dir	= sim_dir+'/'+machine+'/'+shot+'/'+casex+'/'+run+'/'+postproc_dir

	;load_target_geo, base_dir
	load_target_data, base_dir
	plot_target_data, data_id, limiter=limiter
	;plot_target, data_id, 0

	if (check_button('save_output')) then device, /close
        print,"... done!"
end; plot_depo_data
;=======================================================================
