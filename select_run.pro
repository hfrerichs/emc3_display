;-----------------------------------------------------------------------
; module:	select_run
; date:		Jul 10th, 2007
;
; author:	Heinke Frerichs
; email:	h.frerichs@fz-juelich.de
;
; description:	provides routines to create run selection and information 
;		interface (upper part of main window).
;-----------------------------------------------------------------------



;=======================================================================
; get selected machine, shot, case and run for plotting
function get_selected_machine, get_id=get_id
	common	event_id_cb
	common	config_cb

	id	= widget_info(1L, find_by_uname=cb_machine_id)
	text	= widget_info(id, /combobox_gettext)

	if keyword_set(get_id) then begin
		idx	= where(machines eq text, count)
		return, (count eq 0) ? -1 : idx(0)
	end

	return, text
end; get_selected_machine
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
function get_selected_shot
	common	event_id_cb

	id	= widget_info(1L, find_by_uname=cb_shot_id)
	text	= widget_info(id, /combobox_gettext)
	return, text
end; get_selected_shot
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
function get_selected_case, cmp=cmp
	common	event_id_cb

	if keyword_set(cmp) then uname='cb_cmp_case' else uname=cb_case_id
	id	= widget_info(1L, find_by_uname=uname)
	text	= widget_info(id, /combobox_gettext)
	return, text
end; get_selected_case
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
function get_selected_run, cmp=cmp
	common	event_id_cb

	if keyword_set(cmp) then uname='cb_cmp_run' else uname=cb_run_id
	id	= widget_info(1L, find_by_uname=uname)
	text	= widget_info(id, /combobox_gettext)
	return, text
end; get_selected_run
;=======================================================================



;=======================================================================
; return string arrays with available machines, shots, cases and runs in simulation directory
function get_machines
	common	config_cb
	str_array	= file_basename(file_search(sim_dir+'/*', /test_directory))

	if size(str_array, /dimensions) eq 0 then str_array = [' ']
	return, str_array
end; get_machines
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
function get_shots
	common	config_cb
	machine		= get_selected_machine()
	str_array	= file_basename(file_search(sim_dir+'/'+machine+'/*', /test_directory))

	if size(str_array, /dimensions) eq 0 then str_array = [' ']
	return, str_array
end; get_shots
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
function get_cases
	common	config_cb
	machine		= get_selected_machine()
	shot		= get_selected_shot()
	str_array	= file_basename(file_search(sim_dir+'/'+machine+'/'+shot+'/*', /test_directory))

	if size(str_array, /dimensions) eq 0 then str_array = [' ']
	; exclude geometry_dir from list
	idx		= where(str_array ne geometry_dir, count)
	str_array	= (count eq 0) ? [' '] : str_array[idx]

	return, str_array
end; get_cases
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
function get_runs, cmp=cmp
	common	config_cb
	machine		= get_selected_machine()
	shot		= get_selected_shot()
	casex		= get_selected_case(cmp=cmp)
	str_array	= file_basename(file_search(sim_dir+'/'+machine+'/'+shot+'/'+casex+'/*', /test_directory))

	if size(str_array, /dimensions) eq 0 then str_array = [' ']
	; exclude run_exec_dir from list
	idx		= where(str_array ne run_exec_dir, count)
	str_array	= (count eq 0) ? [' '] : str_array[idx]

	return, str_array
end; get_runs
;=======================================================================



;=======================================================================
; read case information
pro read_case_info
	common	config_cb
	common	case_info_cb, $
		case_info, $
		niont, text_ion, nstage, atmass, NPSPEI, $
		D_perp, chi_e, chi_i, $
		nbund_tye, cbund_coe, lambda_n, lambda_T, P_heat
	common	grid_info_cb

	case_dir	= sim_dir+'/'+get_selected_machine()+'/'+get_selected_shot()+'/'+get_selected_case()
	case_file	= case_dir+'/'+run_exec_dir+'/'+case_info_file

	niont	= 0
	NPSPEI	= 0
; test if info file exists
	if not file_test(case_file) then begin
		print, 'case information file not found: ', case_file
		case_info	= 0
		return
	endif

; read info file
	openr, lun, case_file, /get_lun

	; total number of ion species
	reads, scrape(lun), niont

	text_ion	= make_array(niont, /str)
	nstage		= make_array(niont, /int)
	atmass		= make_array(niont, /fl)

	; ion species data
	a	= ''
	b	= 0
	c	= 0.0
	for i=0,niont-1 do begin
		reads, scrape(lun), a, b, c, format='(A6,I6,F12.5)'
		text_ion(i)	= a
		nstage(i)	= b
		atmass(i)	= c
		NPSPEI		= NPSPEI + nstage(i)
	endfor

	; transport coefficients
	D_perp		= make_array(niont, /fl)
	for i=0,niont-1 do begin
		reads, scrape(lun), c
		D_perp(i)	= c
	endfor
	reads, scrape(lun), chi_e, chi_i

	; boundary conditions for particles
	nbund_tye	= make_array(niont, /int)
	cbund_coe	= make_array(niont, /fl)
	nbsrfp		= make_array(niont, /int)
	a	= 0
	b	= 0.0
	lambda_n	= 0.0
	for i=0,niont-1 do begin
		reads, scrape(lun), a, b
		nbund_tye(i)	= a
		cbund_coe(i)	= b

		reads, scrape(lun), a
		nbsrfp(i)	= a
		for j=0,a-1 do begin
			reads, scrape(lun), isn, inr, ids, coi
			; set efolding length for density
			if (ids eq -2) then begin
				if ((lambda_n ne 0.0) and (lambda_n ne coi)) then print, 'warning: efolding length lambda_n not unique!'
				lambda_n	= coi
			endif
		endfor
	endfor

	; boundary conditions for energy and momentum
	P_heat	= 0.0
	lambda_T	= 0.0
	reads, scrape(lun), a
	for j=0,a-1 do begin
		reads, scrape(lun), isn, inr, ids, coe, coi
		; set total heating power
		P_heat	= P_heat + coe + coi
		; set efolding length for density
		if (ids eq -2) then begin
			if ((lambda_T ne 0.0) and (lambda_T ne coi) or (coe ne coi)) then print, 'warning: efolding length lambda_T not unique!'
			lambda_T	= coi
		endif
	endfor

	free_lun, lun
	case_info	= 1
end; read_case_info
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
pro read_notes, dir
	common	config_cb
	common	case_info_cb
	common	event_id_cb

; set run date
	id		= widget_info(1L, find_by_uname=notes_id)
	dir		= sim_dir+'/'+get_selected_machine()+'/'+get_selected_shot()
	info		= file_info(dir+'/'+get_selected_case()+'/'+get_selected_run())
	widget_control, id, set_value='run date: '+string(systime(0,info.ctime))
	widget_control, id, set_value='', /append
	str	= ''

; shot specific notes
	data_file	= dir+'/'+note_file
	; test if file exists
	if file_test(data_file) then begin
		; read notes
		openr, lun, data_file, /get_lun
		while ~ eof(lun) do begin
			readf, lun, str
			widget_control, id, set_value=str, /append
		endwhile
		free_lun, lun
	endif


; case specific notes
	data_file	= dir+'/'+get_selected_case()+'/'+run_exec_dir+'/'+note_file
	; test if file exists
	if file_test(data_file) then begin
		; read notes
		openr, lun, data_file, /get_lun
		while ~ eof(lun) do begin
			readf, lun, str
			widget_control, id, set_value=str, /append
		endwhile
		free_lun, lun
	endif
end; read_notes
;=======================================================================



;=======================================================================
pro show_case_info
	common	case_info_cb
	common	event_id_cb


	id	= widget_info(1L, find_by_uname=case_info_id)
	widget_control, id, set_value=''
	if (case_info eq 0) then return

	;if (niont eq 0) then return
	;widget_control, id, set_value='main plasma component:'
	;widget_control, id, set_value=text_ion(0)+string(atmass(0)), /append
	;widget_control, id, set_value='', /append
;
	;if (niont gt 1) then begin
		;widget_control, id, set_value=strcompress('with '+string(niont-1)+' impurity species:'), /append
		;for i=1,niont-1 do begin
			;widget_control, id, set_value=text_ion(i)+string(atmass(i)), /append
		;endfor
	;endif
	
	widget_control, id, set_value='boundary conditions:'
	bc_str	= ''
	if (nbund_tye(0) eq 1) then bc_str	= '    n_in = '
	if (nbund_tye(0) eq 0) then bc_str	= '    gamma_p = '
	bc_str	= bc_str+string(cbund_coe(0),format='(e8.2)')+' cm^-3'+$
			',    P_heat = '+string(P_heat,format='(e8.2)')+' W'
	widget_control, id, set_value=bc_str, /append
	widget_control, id, set_value='', /append
	widget_control, id, set_value='transport coefficients:', /append
	tc_str	= '    D = '+string(D_perp(0)/1.e4,format='(f4.2)')+' m^2/s'+$
			',    chi_e = '+string(chi_e/1.e4,format='(f4.2)')+' m^2/s'+$
			',    chi_i = '+string(chi_i/1.e4,format='(f4.2)')+' m^2/s'
	widget_control, id, set_value=tc_str, /append
	widget_control, id, set_value='', /append
	widget_control, id, set_value='efolding lengths:', /append
	el_str	= '    l_n = '+string(lambda_n,format='(f3.1)')+' cm'+$
			',    l_T = '+string(lambda_T,format='(f3.1)')+' cm'
	widget_control, id, set_value=el_str, /append

end; show_case_info
;=======================================================================



;=======================================================================
pro update_machine
	common	event_id_cb
	common	config_cb

	new_machine	= get_selected_machine()
	if (last_machine eq new_machine) then return

	last_machine	= new_machine
	id	= widget_info(1L, find_by_uname=cb_shot_id)
	widget_control, id, set_value=get_shots()

	; update pre-defined zooms
	update_zooms

	update_shot
end; update_machine
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
pro update_shot
	common	event_id_cb
	common	config_cb

	new_shot	= get_selected_shot()
	if (last_shot eq new_shot) then return

	last_shot	= new_shot
	id	= widget_info(1L, find_by_uname=cb_case_id)
	widget_control, id, set_value=get_cases()

	id	= widget_info(1L, find_by_uname='cb_cmp_case')
	widget_control, id, set_value=get_cases()

	grid_dir	= sim_dir+'/'+get_selected_machine()+'/'+get_selected_shot()
	read_grid_info, grid_dir, iflag

	update_case
	update_1D_plot_types
end; update_shot
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
pro update_case
	common	event_id_cb
	common	config_cb

	new_case	= get_selected_case()
	;if (last_case eq new_case) then return

	last_case	= new_case
	id		= widget_info(1L, find_by_uname=cb_run_id)
	runs		= get_runs()
	widget_control, id, set_value=runs, set_combobox_select=size(runs, /n_elements)-1

	;id		= widget_info(1L, find_by_uname='cb_cmp_run')
	;runs		= get_runs(/cmp)
	;widget_control, id, set_value=runs, set_combobox_select=size(runs, /n_elements)-2

	read_case_info
	show_case_info
	;update_impurity

	update_run
end; update_case
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
pro update_cmp_case
	common	event_id_cb
	common	config_cb

	id		= widget_info(1L, find_by_uname='cb_cmp_run')
	runs		= get_runs(/cmp)
	widget_control, id, set_value=runs, set_combobox_select=size(runs, /n_elements)-2
end; update_cmp_case
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
pro update_run
	read_notes

	check_data
end; update_run
;=======================================================================



;=======================================================================
; check if data files for selected run exist
pro check_data
	common	config_cb
	common	case_info_cb
	common	event_id_cb

	machine		= get_selected_machine()
	shot		= get_selected_shot()
	casex		= get_selected_case()
	run		= get_selected_run()
	geo_dir		= sim_dir+'/'+machine+'/'+shot+'/'+geometry_dir+'/'
	run_dir		= sim_dir+'/'+machine+'/'+shot+'/'+casex+'/'+run+'/'

	; check plasma data
	n_pl	= (size(plasma_arr, /dimensions))[1]
	for i=0,n_pl-1 do begin
		data_file	= run_dir+plasma_arr[1,i]
		uname		= 'pl_id.'+strcompress(string(i), /remove_all)

		id		= widget_info(1L,find_by_uname=uname)
		widget_control, id, sensitive=file_test(data_file)
	endfor

	; check magnetic data
	n_mg	= (size(magnetic_arr, /dimensions))[1]
	for i=0,n_mg-1 do begin
		data_file	= geo_dir+magnetic_arr[1,i]
		uname		= 'mg_id.'+strcompress(string(i), /remove_all)

		id		= widget_info(1L,find_by_uname=uname)
		widget_control, id, sensitive=file_test(data_file)
	endfor

	; check diagnostic data
	n_dg	= (size(diagnostic_arr, /dimensions))[1]
	for i=0,n_dg-1 do begin
		data_file	= run_dir+diagnostic_arr[1,i]
		uname		= 'dg_id.'+strcompress(string(i), /remove_all)

		id		= widget_info(1L,find_by_uname=uname)
		widget_control, id, sensitive=file_test(data_file)
	endfor

	; check impurities
	id	= widget_info(1L,find_by_uname='imp_select_id')
	if (niont gt 1) then begin
		imp_strs	= make_array(NPSPEI-1, /string)
		k		= 0
		for i=1,niont-1 do begin
		for j=0,nstage(i)-1 do begin
			imp_strs[k]	= strcompress(text_ion(i)) + ' '+strcompress(string(j+1), /remove_all) + '+ '
			k	= k + 1
		endfor
		endfor
		widget_control, id, set_value=imp_strs
		widget_control, id, sensitive=1
	end else begin
		widget_control, id, sensitive=0
	endelse


	; check deposition data
	data_file	= run_dir+postproc_dir+'/'+depo_file(2)
	n_dp	= (size(depo_arr, /dimensions))[1]
	for i=0,n_dp-1 do begin
		uname		= 'dp_id.'+strcompress(string(i), /remove_all)

		id		= widget_info(1L,find_by_uname=uname)
		widget_control, id, sensitive=file_test(data_file)
	endfor

	id		= widget_info(1L, find_by_uname='lim_select_id')
	if (file_test(data_file)) then begin
		load_target_info, run_dir+postproc_dir, labels
		widget_control, id, set_value=labels
	endif else begin
		widget_control, id, set_value='                                '
	endelse

	; check limiter

end; check_data
;=======================================================================



;=======================================================================
; create base with run selection interface
function make_select_run_base, top_base0
	common	event_id_cb
	common	config_cb

	top_base	= widget_base(top_base0, column=1, xpad=0, ypad=0)
	label		= widget_label(top_base, value='select run:', /align_left, resource_name='frame_title')
	base		= widget_base(top_base, row=4, title='select_run', frame=1, resource_name='select_run_base')

	; select machine
	l_machine	= widget_label(base, value='machine:    ', xsize=100, /align_right)
	machines	= ['                  ']
	cb_machine	= widget_combobox(base, value=machines, xsize=240, uname=cb_machine_id)
	machines	= get_machines()
	widget_control, cb_machine, set_value=machines
	; set machine from history
	for i = size(machines, /n_elements)-1,0,-1 do begin
		if (machines[i] eq last_machine) then break
	endfor
	widget_control, cb_machine, set_combobox_select=i

	; select shot
	l_shot		= widget_label(base, value='shot:    ', xsize=100, /align_right)
	shots		= get_shots()
	cb_shot		= widget_combobox(base, value=shots, xsize=240, uname=cb_shot_id)
	; set shot from history
	for i = size(shots, /n_elements)-1,0,-1 do begin
		if (shots[i] eq last_shot) then break
	endfor
	widget_control, cb_shot, set_combobox_select=i

	; select case
	l_case		= widget_label(base, value='case:    ', xsize=100, /align_right)
	cases		= get_cases()
	cb_case		= widget_combobox(base, value=cases, xsize=240, uname=cb_case_id)
	; set case from history
	for i = size(cases, /n_elements)-1,0,-1 do begin
		if (cases[i] eq last_case) then break
	endfor
	widget_control, cb_case, set_combobox_select=i

	; select run
	l_run		= widget_label(base, value='run:    ', xsize=100, /align_right)
	runs		= get_runs()
	cb_run		= widget_combobox(base, value=runs, xsize=240, uname=cb_run_id)
	; set run from history
	for i = size(runs, /n_elements)-1,0,-1 do begin
		if (runs[i] eq last_run) then break
	endfor
	widget_control, cb_run, set_combobox_select=i

	current_geometry_dir	= sim_dir+'/'+get_selected_machine()+'/'+get_selected_shot()+'/'+geometry_dir+'/'
	return, base
end; make_select_run_base
;=======================================================================



;=======================================================================
; create bases with case information and notes
function make_case_info_base, top_base
	common	event_id_cb

	base		= widget_base(top_base, /grid_layout, frame=1)
	case_info	= widget_text(base, uname=case_info_id, xsize=64, ysize=9, resource_name='case_info')

	return, base
end; make_case_info_base

function make_notes_base, top_base
	common	event_id_cb

	base		= widget_base(top_base, /grid_layout, frame=1)
	case_info	= widget_text(base, uname=notes_id, xsize=48, ysize=9, resource_name='case_info')

	return, base
end; make_note_base
;=======================================================================



;=======================================================================
function make_diff_base, top_base

	base		= widget_base(top_base, row=1, frame=1, xoffset=3, xpad=0, ypad=0, resource_name='diff_base')
	toggle_base	= widget_base (base, column=1, /nonexclusive, xpad=0, ypad=0)
	toggle_button	= widget_button(toggle_base, value='compare to:', uname='compare_runs')

	l_case_comp	= widget_label(base, value='case: ', /align_right, sensitive=0, uname='l_cmp_case', xsize=120)
	cases		= get_cases()
	cb_case		= widget_combobox(base, value=cases, sensitive=0, uname='cb_cmp_case', xsize=240)

	l_run_comp	= widget_label(base, value='run: ', /align_right, sensitive=0, uname='l_cmp_run', xsize=120)
	runs		= get_runs(/cmp)
	cb_run		= widget_combobox(base, value=runs, sensitive=0, uname='cb_cmp_run', xsize=240)

	diff_strs	= ['absolute difference', 'rel. diff. (F1-F2)/F1', 'rel. diff. (F1-F2)/F2', 'rel. diff. 2*(F1-F2)/(F1+F2)']
	cb_diff		= widget_combobox(base, value=diff_strs, sensitive=0, uname='cb_cmp_diff', xsize=240)
	return, base
end; make_diff_base
;=======================================================================



;=======================================================================
; create main base for run selection interface
function make_run_base, top_base
	common	config_cb

	vbase		= widget_base(top_base, row=2, frame=1)

	; 1st row
	hbase		= widget_base(vbase, column=3, xpad=0, ypad=0)
	select_base	= make_select_run_base(hbase)
	info_base	= make_case_info_base(hbase)
	notes_base	= make_notes_base(hbase)

	; 2nd row
	diff_base	= make_diff_base(vbase)


	read_case_info
	show_case_info
	read_notes

	return, vbase
end; make_run_base
;=======================================================================
