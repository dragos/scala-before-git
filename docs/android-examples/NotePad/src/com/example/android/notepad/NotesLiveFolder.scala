/*
 * Copyright (C) 2009 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.example.android.notepad

import _root_.android.app.Activity
import _root_.android.app.Activity._
import _root_.android.content.Intent
import _root_.android.net.Uri
import _root_.android.os.Bundle
import _root_.android.provider.LiveFolders

object NotesLiveFolder {
  /**
   * The URI for the Notes Live Folder content provider.
   */
  final val CONTENT_URI = Uri.parse("content://"
    + NotePad.AUTHORITY + "/live_folders/notes")

  final val NOTE_URI = Uri.parse("content://"
    + NotePad.AUTHORITY + "/notes/#")
    
}

class NotesLiveFolder extends Activity {
  import NotesLiveFolder._ // companion object

  override protected def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)

    val intent = getIntent()
    val action = intent.getAction()

    if (LiveFolders.ACTION_CREATE_LIVE_FOLDER equals action) {
      // Build the live folder intent.
      val liveFolderIntent = new Intent()

      liveFolderIntent.setData(CONTENT_URI)
        liveFolderIntent.putExtra(LiveFolders.EXTRA_LIVE_FOLDER_NAME,
                getString(R.string.live_folder_name))
        liveFolderIntent.putExtra(LiveFolders.EXTRA_LIVE_FOLDER_ICON,
                Intent.ShortcutIconResource.fromContext(this,
                        R.drawable.live_folder_notes))
        liveFolderIntent.putExtra(LiveFolders.EXTRA_LIVE_FOLDER_DISPLAY_MODE,
                LiveFolders.DISPLAY_MODE_LIST)
        liveFolderIntent.putExtra(LiveFolders.EXTRA_LIVE_FOLDER_BASE_INTENT,
                new Intent(Intent.ACTION_EDIT, NOTE_URI))

      // The result of this activity should be a live folder intent.
      setResult(RESULT_OK, liveFolderIntent)
    } else {
      setResult(RESULT_CANCELED)
    }

    finish()
  }
}
