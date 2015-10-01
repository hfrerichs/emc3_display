;-----------------------------------------------------------------------
function check_button, widget_id
        return, widget_info(widget_info(1L, find_by_uname=widget_id), /button_set)
end; function check_button
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro toggle_sensitive, trigger_uname, uname
        c_id		= widget_info(1L, find_by_uname=uname)
        widget_control, c_id, sensitive=check_button(trigger_uname)
end; toggle_sensitive
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
function get_entry, uname, id1st=id1st
	tmp	= ''
	if keyword_set(id1st) then start_id=id1st else start_id=1L
	widget_control, widget_info(start_id, find_by_uname=uname), get_value=tmp
	return, tmp
end; function get_entry
;-----------------------------------------------------------------------
pro set_entry, uname, value, id1st=id1st
	if keyword_set(id1st) then start_id=id1st else start_id=1L
	widget_control, widget_info(start_id, find_by_uname=uname), set_value=value
	return
end; pro set_entry
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; get data from widget uname and store it to var
function copy_data, var, uname, id1st=id1st
	str	= (get_entry(uname, id1st=id1st))[0]
	if str ne '' then begin
		var = double(str)
		print, uname, ' = ', var
		return, 1
	endif
	return, 0
end; copy_data
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
function scrape, lun, cmask=cmask
	if  not keyword_set(cmask) then cmask = '*'
	str	= ''
	repeat begin
		readf, lun, str
	endrep until (strmid(str,0,1) ne cmask)
	return, str
end; function scrape
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro enforce_float, uname
        c_id		= widget_info(1L, find_by_uname=uname)
        widget_control, c_id, get_value=value
        widget_control, c_id, set_value=float(value)
end; enforce_float
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
; returns number of selected item in a droplist
function get_select, widget_uname
	return, widget_info (widget_info (1L, find_by_uname=widget_uname), /droplist_select)
end; function get_select
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
function get_combobox_text, widget_uname
	return, widget_info (widget_info (1L, find_by_uname=widget_uname), /combobox_gettext)
end; function get_combobox_text
;-----------------------------------------------------------------------
