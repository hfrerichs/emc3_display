;-----------------------------------------------------------------------
; module:	fl_profiles
; date:		Apr 03th, 2008
;
; author:	Heinke Frerichs
; email:	hfrerichs@wisc.edu
;
; description:	fieldline profiles
;-----------------------------------------------------------------------



;=======================================================================
pro load_fieldlines
	common	config_cb
	common	bfls_cb,	BFLS_N, BFLS_NCELLS, BFLS_LEN_PTR, BFLS_ID_PTR, BFLS_RMIN_PTR
        BFL_NC_MAX = 1000

	base		= widget_base(column=1, title='select fieldline')
	slider		= widget_slider(base, minimum=0, maximum=100)
	widget_control, base, /realize

        BFLS_NPLTT = 0L
        openr, lun, current_geometry_dir+fl_profiles_file, /get_lun
        readf, lun, BFLS_NPLTT
        print, "Found NPLTT=", BFLS_NPLTT
        dummy1=0L
        dummy2=0L
        dummy3=0L
        dummy4=0L
        for i=1,BFLS_NPLTT do begin
                readf, lun, dummy1, dummy2
        endfor
        readf, lun, BFL_SUBRES_I, BFL_SUBRES_J
        print, "Found Subres: ", BFL_SUBRES_I, BFL_SUBRES_J


        BFLS_N     = BFLS_NPLTT*BFL_SUBRES_I*BFL_SUBRES_J
        BFLS_NCELLS= INTARR(BFLS_N)
        BFLS_RC1   = DBLARR(BFLS_N)
        BFLS_ZC1   = DBLARR(BFLS_N)
        BFLS_PHIC1 = DBLARR(BFLS_N)
        BFLS_RC2   = DBLARR(BFLS_N)
        BFLS_ZC2   = DBLARR(BFLS_N)
        BFLS_PHIC2 = DBLARR(BFLS_N)
        BFLS_LEN_PTR = PTRARR(BFLS_N, /ALLOCATE_HEAP)
        BFLS_ID_PTR  = PTRARR(BFLS_N, /ALLOCATE_HEAP)
        BFLS_RMIN_PTR  = PTRARR(BFLS_N, /ALLOCATE_HEAP)

        ; over read bf_lines
        dummyArrR  = dblarr(BFL_NC_MAX)
        dummyArrI  = intarr(BFL_NC_MAX)
        dummyChar  ='                                                                             '
        dummyArrR6 = dblArr(6)
        print, FORMAT='(a,I0,a)', "Reading ", BFLS_N, " Filed Lines..."
        for k=0L,BFLS_N-1L do begin
;for i=1,BFL_SUBRES_I do begin
;for j=1,BFL_SUBRES_J do begin
                readf, lun, dummyChar
;print,dummyChar
                readf, lun, dummyChar
;print,dummyChar
                readf, lun, dummy1
                BFLS_NCELLS[k]= dummy1
;print,dummy1
                readf, lun, dummyChar
;print,dummyChar
                readf, lun, dummyChar
;    print, dummyChar
                readf, lun, dummyArrR6

                BFLS_RC1[k]   = dummyArrR6[0]
                BFLS_ZC1[k]   = dummyArrR6[1]
                BFLS_PHIC1[k] = dummyArrR6[2]
                BFLS_RC2[k]   = dummyArrR6[3]
                BFLS_ZC2[k]   = dummyArrR6[4]
                BFLS_PHIC2[k] = dummyArrR6[5]
;    print, BFLS_RC1[k], BFLS_ZC1[k], BFLS_PHIC1[k], BFLS_RC2[k], BFLS_ZC2[k], BFLS_PHIC2[k]
;print,dummyChar
                dummyArrR     = dblarr(dummy1)
                dummyArrI     = lonarr(dummy1)
                readf, lun, dummyChar
                readf, lun, dummyArrR
                *BFLS_LEN_PTR[k] = dummyArrR

                readf, lun, dummyChar
                readf, lun, dummyArrI
                *BFLS_ID_PTR[k]  = dummyArrI

                ;readf, lun, dummyChar
                ;readf, lun, dummyArrR
                ;*BFLS_RMIN_PTR[k]  = dummyArrR
;print,dummyArrI
;endfor ;j
;endfor ;iBFLS_RC1[k], BFLS_ZC1[k], BFLS_PHIC1[k], 
		widget_control, slider, set_value=fix(100*k/(BFLS_N-1L))
        endfor ;k

        free_lun, lun
        print, "...done"
	widget_control, base, /destroy

	loaded_shot_fl	= get_selected_machine()+'/'+get_selected_shot()
	id	= widget_info (1L, find_by_uname='import_fl')
	widget_control, id, sensitive=0
end; load_fieldlines
;=======================================================================



;=======================================================================
pro load_fieldline, input_file
	common	fl_cb, n_seg, l_pos, id_cell

	print, 'loading fieldline from file: ', input_file
	openr, lun, input_file, /get_lun
	n_seg	= 0
	str	= ''
	readf, lun, n_seg

	l_pos	= make_array(n_seg, /fl)
	id_cell	= make_array(n_seg, /long)
	readf, lun, str
	readf, lun, l_pos
	readf, lun, str
	readf, lun, id_cell
	free_lun, lun
end; load_fieldline
;=======================================================================



;=======================================================================
pro select_cell, physQ
	common	grid_info_cb
	common	grid_data_cb
	common	bfls_cb

	nr	= 24
	np	= 30
	nt	= 8
	nz	= 0
	mesh_id	= 0L + nr + (np + zon_polo(nz)*nt) * zon_radi(nz) + mesh_p_os(nz)
	grid_id	= 0L + nr + (np + srf_polo(nz)*nt) * srf_radi(nz) + grid_p_os(nz)
	cell_id	= 0L + idcell(mesh_id)
	;print, 'mesh_id = ', mesh_id
	;print, 'cell_id = ', cell_id

	print, 'R = ', rg(grid_id), ', Z = ', zg(grid_id)


	i	= 0L
	print, 'searching for cell index: ', cell_id
	for k=0L,BFLS_N-1L do begin
        	LENGTHS = *BFLS_LEN_PTR[k]
        	IDS     = *BFLS_ID_PTR[k]
		for n=0L,BFLS_NCELLS[k]-1 do begin
			if (IDS(n) eq cell_id) then begin
				plot, LENGTHS-LENGTHS(n), physQ(IDS-1), /noerase
				i = i + 1
				break
			endif
		endfor
	endfor
	print, 'found ', i
	print, '... done'

end; select_cell

pro plot_fl_profile, physQ
	common	bfls_cb

	print, 'now plotting fieldline profile'
	;select_cell, physQ

	bfls_select	= 0
	bfls_select	= (long(get_entry('select_fl')))[0]
	k		= bfls_select
        LENGTHS = *BFLS_LEN_PTR[k]
        IDS     = *BFLS_ID_PTR[k]

	plot, LENGTHS, physQ(IDS-1)

	openw, lun, 'fl_profile.txt', /get_lun
	for i=0,BFLS_NCELLS[k]-1 do begin
		printf, lun, LENGTHS(i), physQ(IDS(i)-1)
	endfor
	free_lun, lun

	;openw, lun, 'fl_geo.txt', /get_lun
	;for i=0,*BFLS_NCELLS[k]-1 do begin
	;	printf, lun, LENGTHS(i), physQ(IDS-1)
	;endfor
	;free_lun, lun
end;

pro plot_fl_profile_old, physQ
	common	fl_cb

	print, 'now plotting fieldline profile'

	;print, id_cell
	;physQ_fl	= make_array(n_seg, /fl)
	;for i=0,n_seg-1 do begin
		;physQ_fl(i)	= physQ(id_cell(i)-1)
	;endfor

	plot, l_pos, physQ(id_cell-1)

	openw, lun, 'fl_profile.txt', /get_lun
	for i=0,n_seg-1 do begin
		printf, lun, l_pos(i), physQ(id_cell(i)-1)
	endfor
	free_lun, lun
end; plot_fl_profile
;=======================================================================



;=======================================================================
pro load_fieldline_event, event
	common	config_cb

   WIDGET_CONTROL, event.top, GET_UVALUE=state, /NO_COPY  
  
   CASE event.DONE OF  
      0: BEGIN  
            state.file = event.VALUE  
            WIDGET_CONTROL, event.top, SET_UVALUE=state, /NO_COPY  
         END  
      1: BEGIN  
		input_file	= ''
            IF (state.file NE '') THEN input_file = state.file
            WIDGET_CONTROL, event.top, SET_UVALUE=state, /NO_COPY  
            IF (input_file NE '') THEN BEGIN  
		load_fieldline, input_file
		WIDGET_CONTROL, event.top, /DESTROY  
		fieldline_ui = 0
            ENDIF  
         END  
      2: BEGIN
		WIDGET_CONTROL, event.top, /DESTROY  
		fieldline_ui = 0
         end
   ENDCASE  
	;event_id	= widget_info(event.id, /uname)

	;print, event_id
end; load_fieldline_event
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
pro load_fieldline_ui
	common	config_cb

	if (fieldline_ui ne 0) then return;	ui already running

	base		= widget_base(column=1, title='select fieldline')
	file_sel	= cw_filesel(base, uname='fsel')
	file		= ''
	state		= {file:file}

	widget_control, base, /realize
	widget_control, base, set_uvalue=state, /no_copy
	fieldline_ui	= base
	xmanager, 'load_fieldline', base
end; load_fieldline_ui
;=======================================================================
