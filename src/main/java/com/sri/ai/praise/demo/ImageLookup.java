/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.praise.demo;

import javax.swing.ImageIcon;

import com.google.common.annotations.Beta;

/**
 * 
 * @author oreilly
 *
 */
@Beta
public class ImageLookup {	
	public static final ImageIcon NEW_LARGE             = createImageIcon("document-new32x32.png");
	public static final ImageIcon OPEN_LARGE            = createImageIcon("document-open32x32.png");
	public static final ImageIcon SAVE_LARGE            = createImageIcon("document-save32x32.png");
	public static final ImageIcon SAVE_AS_LARGE         = createImageIcon("document-save-as32x32.png");
	public static final ImageIcon SAVE_ALL_LARGE        = createImageIcon("document-save-all32x32.png");
	public static final ImageIcon UNDO_LARGE            = createImageIcon("edit-undo32x32.png");
	public static final ImageIcon REDO_LARGE            = createImageIcon("edit-redo32x32.png");
	public static final ImageIcon VALIDATE_LARGE        = createImageIcon("document-properties32x32.png");
	public static final ImageIcon EXECUTE_QUERY_LARGE   = createImageIcon("go-next32x32.png"); 
	public static final ImageIcon STOP_QUERY_LARGE      = createImageIcon("process-stop32x32.png");
	public static final ImageIcon CLEAR_LARGE           = createImageIcon("edit-clear32x32.png");
	public static final ImageIcon NEW_WINDOW_LARGE      = createImageIcon("window-new32x32.png");
	public static final ImageIcon PROCESS_WORKING_LARGE = createImageIcon("process-working32x32.png");
	//
	public static final ImageIcon NEW_SMALL             = createImageIcon("document-new22x22.png");
	public static final ImageIcon OPEN_SMALL            = createImageIcon("document-open22x22.png");
	public static final ImageIcon SAVE_SMALL            = createImageIcon("document-save22x22.png");
	public static final ImageIcon SAVE_AS_SMALL         = createImageIcon("document-save-as22x22.png");
	public static final ImageIcon SAVE_ALL_SMALL        = createImageIcon("document-save-all22x22.png");
	public static final ImageIcon UNDO_SMALL            = createImageIcon("edit-undo22x22.png");
	public static final ImageIcon REDO_SMALL            = createImageIcon("edit-redo22x22.png");
	public static final ImageIcon VALIDATE_SMALL        = createImageIcon("document-properties22x22.png");
	public static final ImageIcon EXECUTE_QUERY_SMALL   = createImageIcon("go-next22x22.png"); 
	public static final ImageIcon STOP_QUERY_SMALL      = createImageIcon("process-stop22x22.png");
	public static final ImageIcon CLEAR_SMALL           = createImageIcon("edit-clear22x22.png");
	public static final ImageIcon NEW_WINDOW_SMALL      = createImageIcon("window-new22x22.png");
	public static final ImageIcon PROCESS_WORKING_SMALL = createImageIcon("process-working22x22.png");
	public static final ImageIcon EDIT_COPY_SMALL       = createImageIcon("edit-copy22x22.png");
	public static final ImageIcon EDIT_CUT_SMALL        = createImageIcon("edit-cut22x22.png");
	public static final ImageIcon EDIT_PASTE_SMALL      = createImageIcon("edit-paste22x22.png");
	public static final ImageIcon EDIT_DELETE_SMALL     = createImageIcon("edit-delete22x22.png");
	public static final ImageIcon EDIT_SELECT_ALL_SMALL = createImageIcon("edit-select-all22x22.png");
	
	//
	// PRIVATE
	//
	
	private static ImageIcon createImageIcon(String path) {
	    java.net.URL imgURL = ImageLookup.class.getResource(path);
	    return new ImageIcon(imgURL);
	}
}
