
/* from Arjen Oosterkamp: http://api.jquery.com/serializeArray/ */
(function( $ ){
$.fn.serializeJSON=function() {
var json = {};
jQuery.map($(this).serializeArray(), function(n, i){
json[n['name']] = n['value'];
});
return json;
};
})( jQuery );

var BASE_URL = window.location.pathname;


/**************************************************************/
/* initialization upon load */
$(function() {
    $.blockUI({ message: $("#loadMessage") });
		        //onUnblock: function() { $("#loadMessage").remove(); }});
    
	/* synchronous-*ONLY* setup functions -- i.e. these should
	   NOT make any AJAX calls */
	onPageLoad();

	// check for CONT_URL cookie -- restore if so, before continuing with
	//  rest of initialization
    checkContUrlCookie(
    	function() {
			$.when(
				/* AJAX calls - the UI is only unblocked after all done */	
				startupDelay(),
				makeRequest("refresh")
			).then( $.unblockUI );
		});
	
	//$.taconite.debug = true; 
});



/**************************************************************/
/* setup functions for various pieces of the page */

/* attempts to see if a valid cont-url is stored in the CONT_URL
   cookie, sets CONT_URL to that if so; in either case, when done
   checking, it executes the given startupFunction */
function checkContUrlCookie(startupFunction) {
	var cookval = $.cookie('CONT_URL');
	if (cookval) {
		$.ajax({url: cookval, data: {requesttype: "ping"},
				error: function() {
							$.cookie('CONT_URL', null); /* clear it */ 
							startupFunction();
						},
				success: function() { 
							CONT_URL = cookval;
							startupFunction();
						 }
				});
	} else {
		startupFunction();
	}
}


/* called once, on load 
   this should not make any ajax calls -- only set up gui images/themes,
   and event handlers
*/
function onPageLoad() {
	// tabs setup
  		//var hasResult = $("#program-output").children().length;
	$("#tabs").tabs({selected: 1, disabled: [2]});

	// disable form submission
	$("#edit > form").submit(function() { return false; });

	// button images
	$("#edit-again-button").button({icons: {primary: "ui-icon-wrench"}});
	$("#clear-input-button").button({icons: {primary: "ui-icon-cancel"}});
	$("#save-input-button").button({icons: {primary: "ui-icon-disk"}});
	$("#apply-input-button").button({icons: {primary: "ui-icon-lightbulb"}});
	$("#exit-server").button({icons: {primary: "ui-icon-power"}});
	
	// button handlers	
	$("#apply-input-button").click(makeRequestFunc("apply-input", true, true, 
									{ onStart: $.blockUI, onComplete: $.unblockUI }));
	$("#clear-input-button").click(makeRequestFunc("clear-input", true));
	$("#save-input-button").click(makeRequestFunc("save-input", true));
	$("#edit-again-button").click(function() { resultTabState(false, 1); $("body").scrollTop(0); });
	$("#exit-server").click(doShutDown);

	// save tab stuff
	var selvalfunc = function() {return {name: $("#history-select").val()}};
	$("#history-select").change(makeRequestFunc("preview-saved", selvalfunc, false));
	$("#load-save-edit").click(makeRequestFunc("load-saved-edit", selvalfunc, false));
	$("#load-save-apply").click(makeRequestFunc("load-saved-apply", selvalfunc, false,
												{ onStart: $.blockUI, onComplete: $.unblockUI }));
	$("#delete-one-save").click(makeRequestFunc("remove-saved-one", selvalfunc, false));
	$("#delete-all-save").click(makeRequestFunc("remove-saved-all",
										function() { return ($("#loosematch").is(":checked"))
														  ? { loosematch: "loosematch" } : {}; },
										false));
	$("#loosematch").change(populateSaved);
	$("#usersaveonly").change(populateSaved);
	$("#select-save-prev").click(function() { goPreviousSaved(); return false; });
	$("#select-save-next").click(function() { goNextSaved(); return false; });
	$(document).keydown(function (event) {
		if ($("#tabs").tabs( "option", "selected" ) == 0) {
			if (event.keyCode == 39) { goNextSaved(); return false; }
			else if (event.keyCode == 37) { goPreviousSaved(); return false; }
		}
	});

}

/* type : string, params : object, sendFormData : boolean */
function makeRequest( type, params, sendFormData, myops ) {
	return makeRequestFunc(type, params, sendFormData, myops)();
}

/* myops is an object with options for this function:
{ onStart : function() { ... }
  onComplete: function() { ... }
}

- sendFormData is 'true' by default (if undefined)
- possible to only supply type and sendFormData arguments
*/
function makeRequestFunc( type, params, sendFormData, myops ) {
	return function() {
		if (sendFormData == undefined) { 
			if (params == true || params == false) {
				sendFormData = params; params = {}; 
			} else {
				sendFormData = true;
			}
		}
		var thedata = $.extend({ requesttype: type, conturl: CONT_URL }, 
							   (typeof params == 'function' ? params() : params));
		if (sendFormData) thedata = addFormData(thedata);

		//console.log("request: " + type + "\ncont-url: " + CONT_URL + "\nthedata: "); 
		//console.log(thedata);
		if (myops && myops.onStart) { myops.onStart(); }
		return $.ajax({url: CONT_URL, 
					   data: thedata,
					   error: ajaxErrorHandler,
					   complete: function() { if (myops && myops.onComplete) 
					   							myops.onComplete(); }
					   }); 
	};
}

/* for attempting to restart entire application in case of 
   AJAX error */
function ajaxErrorHandler(jqXHR, textStatus, errorThrown) {
	if (textStatus == 'error' && errorThrown == 'File not found') {
		CONT_URL = BASE_URL;
		$.blockUI({ message: $("#reloadMessage") });
		$.when(
			startupDelay(),
			makeRequest("reload")  // like "refresh" but form data is processed
		).then( $.unblockUI );
	}
}	


function goPrevOrNextSaved(offset) {
	var sel = $("#history-select");
	sel.blur(); // this is because firefox seems to go the opposite
	            // direction if the select element has focus ????
	var selected = $("#history-select option:selected");
	var options = $("#history-select option");
	var index = options.index(selected);
	var nextIndex = Math.max(Math.min(index+offset, options.length-1), 1);
	$(options[nextIndex]).attr('selected', 'selected');
	sel.change();  // update preview
}

function goPreviousSaved() { goPrevOrNextSaved(1); }
function goNextSaved() { goPrevOrNextSaved(-1); }


function resultTabState(enabled, selectOther) {
	$("#tabs").tabs( 'option', 'disabled', [] );
	if (selectOther) 
		$("#tabs").tabs( 'option', 'selected', selectOther );
	else if (enabled) 
		$("#tabs").tabs( 'option', 'selected', 2 );
	else // if disabled, other select not specified, move to 1
		$("#tabs").tabs( 'option', 'selected', 1 );

	if (!enabled)
		$("#tabs").tabs( 'option', 'disabled', [2] );
}

function refreshElements(parentSelector) {
	var selpref = parentSelector ? parentSelector+" " : "";

	//
	fixFileLinks();   // processes the entire DOM

	// restore sortable lists
	if ($(selpref+".sortable").length) {
		$(selpref+".sortable").sortable({handle: ".li-handle", items: "li:not(.nosort)"});
		$(selpref+".sortable").bind("sortupdate", reorderList);
	}
    
	// fix images
	$(selpref+".addbtn").button({icons: {primary: "ui-icon-plusthick"}});
	$(selpref+".delbtn").button({text:false, icons: {primary: "ui-icon-trash"}});
    
    // add handlers for list buttons
	// this should not happen repeatedly on the same button...
	$(selpref+"button.delbtn").bind("click", deleteFromList);
	$(selpref+"button.addbtn").bind("click", addToList);
	
	// add handler for oneof selects
	$(selpref+"select.tfield-oneof").change(onSelectOneOf);
	
	// add handle for all non-file input updates
	var inputs = $(selpref+"input");
	for (var i = 0; i < inputs.length; i++) {
		var inp = $(inputs[i]);
		if (inp.attr('type') == 'hidden') {  // completely ignore
		   // do nothing
		} else if (inp.attr('type') == 'file') {   // file input elts
			var id = inp.attr('id');
			inp.change(makeRequestFunc("notify-upload", 
							function() { return {
											name: $('#'+id).attr('name'),
											filename: $('#'+id).val()
										};}, 
							false));
		} else {  // non-file input elts
			inp.change(makeRequestFunc("update-field",
									{name: inp.attr('name')}));
		} 
	}
}


/* a bit of a hack to get all the href's of <a ...> tags around
   file download links set so that on a right-click-and-save they
   actually work! */
function fixFileLinks() {
	// fix address in all file download anchors
	var filelinks = $("a.filelink");
	///console.log("found " + filelinks.length + " file links");
	for (var i = 0; i < filelinks.length; i++) {
		var lnk = $(filelinks[i]);
		var id = lnk.find("input.id").val();
		var savefile;
		if (lnk.find("input.savefile")) {   // inner hidden input
			savefile = lnk.find("input.savefile").val();
		}
		lnk.attr('href', urlForViewFile(id, savefile));
	}
}


/**************************************************************/
/* event handlers */

/* adds serialized parameters from the form to the given
   object */
function addFormData(obj) {
	return $.extend({}, obj, $("#edit > form").serializeJSON());
}


/* initiates actual file upload, given id/name of
   input element of type file */
function onUpload(id) {
	var elt = $("#edit #" + id);

	//console.log("Starting upload: (" + id + ") " + elt.attr('id') + " |" + elt.val() + "|");
	
	$.ajaxFileUpload({
		url: CONT_URL,
		secureuri: false,
		fileElementId: id,
		dataType: "xml",
		data: { requesttype: "file-upload", name : id },
		success: function(data, status) {
			//console.log('file upload success'); console.log(data); console.log(status);
			$.taconite(data);
		},
		error: function(data, status, e) {
			console.log('file upload error'); console.log(e);
		}
	});
}


/* produces a url for downloading a file */
function urlForViewFile(tfieldid, savefilename) {
	var data = {requesttype: "file-view", name: tfieldid};
	if (savefilename) 
		$.extend(data, {savefile: savefilename});
	var requrl = CONT_URL + '?' + $.param(data);
	return requrl;
}


/* opens a file for user to see */
function viewFile(elt, tfieldid, savefilename) {
	var requrl = urlForViewFile(tfieldid, savefilename);
	//console.log(requrl);
	$.open.newWindow(requrl, {});
	return false;
}

function clearFile(tfieldid) {
	makeRequest("file-clear", { name : tfieldid });
	return false;
}

/* when a select is changed */
function onSelectOneOf(ev) {
	var sel = $(this);
	makeRequest("oneof-change", { name: sel.attr('id'),
								  chosen: sel.val() });
}

/* when a list's "add ..." button is clicked */
function addToList(ev) {
	var btn = $(this); // $(ev.target).closest("button");
	var listname = dropSuffix(btn.attr('id'));
	makeRequest("listof-add", { name: listname });
}

/* when delete button is clicked for an item */
function deleteFromList(ev) {
	var btn = $(this);
	var listname = dropSuffix(btn.closest('ol').attr('id'));
	var index = extractLastIndex(btn.attr('id'));
	makeRequest("listof-delete", { name: listname, item: index });
}

/* when a list item is dragged to another position */
function reorderList(event, ui) {
	var item = ui.item;  // li
	var prevli = item.prev('li');
	var nextli = item.next('li');
	var hiddeninput = item.parent().prev('input');
	var thename = hiddeninput.attr('name');
	
	if (prevli.length || nextli.length) {   // determines if need to renumber
		var prefix = extractPrefix(item.attr('id'));
		var itemidx = parseInt(extractLastIndex(item.attr('id')));
		var previdx = extractLastIndex(prevli.attr('id'));
		var nextidx = extractLastIndex(nextli.attr('id'));
		var newitemidx = 0;
		if (previdx) { 
			if (previdx > itemidx) newitemidx = parseInt(previdx); 
			else newitemidx = parseInt(previdx) + 1; 
		}
		else if (nextidx) { // no previdx only if moving to the very top
			newitemidx = 0;
		}
		//console.log(thename +": swap " + itemidx + " and " + newitemidx);

		makeRequest("listof-reorder", {name:thename, from:itemidx, to:newitemidx});
	}
}


/**************************************************************/
/* utility functions */


/* enforces a minimum loading delay */
function startupDelay() {
	var STARTUP_DELAY = 500;  // minimum "loading" delay in milliseconds

	var dfd = $.Deferred();
	setTimeout(dfd.resolve, STARTUP_DELAY); 
	return dfd.promise();
}

/* updates the CONT_URL global variable, sets cookie
*/
function updateContUrl(newURL) {
	CONT_URL = newURL;
	$.cookie('CONT_URL', CONT_URL);
}

/* convert XML DOM object to string */
function xmlToString(data) {
	return (new XMLSerializer()).serializeToString(data);
}

/* determine if o is a number, or can be cast to one */
function isNumber (o) {
  return ! isNaN (o-0);
}

/**** String manipulation of names ****/

/* extractSuffix("a...-xyz") --> "xyz" */
function extractSuffix(str) {
    if (!str) return str;  /* undefined... */
    var pcs = str.split("-");
    var n = pcs.length;
    return pcs[n-1];
}

/* dropSuffix("a...-xyz") --> "a..."
   dropSuffix("a..-1-2") --> "a..-1-2"
  (i.e. drops character suffixes) */
function dropSuffix(str) {
    var i = str.lastIndexOf("-"); 
	if (!isNumber(str.substr(i+1))) {
		return str.substring(0, i);
    } else {
		return str;
    }
}

/* extractLastIndex("a-1-2-3-xyz") --> 3
   extractLastIndex("a-1-2-3") --> 3     */
function extractLastIndex(str) {
    if (!str) return str;  /* undefined... */
    var pcs = str.split("-");
    var n = pcs.length;
    if (!(isNumber(pcs[n-1]))) { return (pcs[n-2]); }
    else { return (pcs[n-1]); }
}

/* extractPrefix("a-1-2-3-li")  -->  "a-1-2-"
   extractPrefix("a-1-2-3-xyz") -->  "a-1-2-"
   extractPrefix("a-1-2-3") -->  "a-1-2-"
 */
function extractPrefix(str) {
    var i = str.lastIndexOf("-"); 
    if (!isNumber(str.substr(i+1))) {
		return str.substring(0, str.lastIndexOf("-", i-1)+1);
    } else {
		return str.substring(0, i+1);
    }
}



/**************************************************************/
/* saved tab functionality */

function populateSaved() {
	var sel = $("#history-select");
	var usersaveonly = $("#usersaveonly").is(":checked");
	var params = (($("#loosematch").is(":checked"))
				? { requesttype: "list-saved", loosematch: "loosematch" }
				: { requesttype: "list-saved" });
	return $.ajax({
		url: CONT_URL,
		data: $.extend({conturl: CONT_URL}, params),
		error: ajaxErrorHandler,
		success: function(data) {
			sel.empty();   // clear all current stuff
			var grps = $(data).find("group");
			for (var g = 0; g < grps.length; g++) {
				var optgrp = $("<optgroup />");
				optgrp.attr("label", $(grps[g]).attr("datestring"));
				var svs = $(grps[g]).find("savefile");
				for (var i = 0; i < svs.length; i++) {
					var elt = $(svs[i]);
					var namestr = elt.attr('name');
					var timestr = elt.attr('timestring');
					var usersave = elt.attr('usersaved');
					if (usersave)
						timestr = "* " + timestr + " *";
					var newop = '<option value="' + namestr
								+ '"'
								+ (usersave ? ' class="usersave"' : '')
								+ '>' + timestr + '</option>';
					if (!usersaveonly || usersave)
						optgrp.prepend(newop);   // to get in reverse order
				}
				if (optgrp.children().length)
					sel.prepend(optgrp); // to get in reverse order by date
			}
			sel.prepend('<option value="-">-</option>');
			//alert((new XMLSerializer()).serializeToString(data));
			sel.change();  // update preview
		},
		dataType: "xml" 
	});
}


/**************************************************************/
/* shutdown */

function doShutDown() {
	$.cookie('CONT_URL', null);
	$("#content").html("<div> </div>");   // clear all content
	$(this).remove();   // remove the "exit" button
	//$.blockUI.defaults.overlayCSS.opacity = 1;  // make overlay very opaque
	$.blockUI({ message: $("#shutdownMessage") });
	$.get("/quit");
}



