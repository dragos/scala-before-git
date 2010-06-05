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

package com.example.android.contactmanager

import android.app.Activity
import android.content.Intent
import android.database.Cursor
import android.net.Uri
import android.os.Bundle
import android.provider.ContactsContract
import android.util.Log
import android.view.View
import android.widget.{Button, CheckBox, CompoundButton, ListView, SimpleCursorAdapter}
import android.widget.CompoundButton.OnCheckedChangeListener

object ContactManager {
  final val TAG = "ContactManager"
}

final class ContactManager extends Activity {
  import ContactManager._ // companion object

  private var mAddAccountButton: Button = _
  private var mContactList: ListView = _
  private var mShowInvisible: Boolean = _
  private var mShowInvisibleControl: CheckBox = _

  /**
   * Called when the activity is first created. Responsible for initializing the UI.
   */
  override def onCreate(savedInstanceState: Bundle) {
    Log.v(TAG, "Activity State: onCreate()")
    super.onCreate(savedInstanceState)
    setContentView(R.layout.contact_manager)

    // Obtain handles to UI objects
    mAddAccountButton = findViewById(R.id.addContactButton).asInstanceOf[Button]
    mContactList = findViewById(R.id.contactList).asInstanceOf[ListView]
    mShowInvisibleControl = findViewById(R.id.showInvisible).asInstanceOf[CheckBox]

    // Initialize class properties
    mShowInvisible = false
    mShowInvisibleControl setChecked mShowInvisible

    // Register handler for UI elements
    mAddAccountButton.setOnClickListener(new View.OnClickListener() {
      def onClick(v: View) {
        Log.d(TAG, "mAddAccountButton clicked")
        launchContactAdder()
      }
    })
    mShowInvisibleControl.setOnCheckedChangeListener(new OnCheckedChangeListener() {
      def onCheckedChanged(buttonView: CompoundButton, isChecked: Boolean) {
        Log.d(TAG, "mShowInvisibleControl changed: " + isChecked)
        mShowInvisible = isChecked
        populateContactList()
      }
    })

    // Populate the contact list
    populateContactList()
  }

  /**
   * Populate the contact list based on account currently selected in the account spinner.
   */
  private def populateContactList() {
    // Build adapter with contact entries
    val cursor = getContacts()
    val fields = Array(ContactsContract2.Data.DISPLAY_NAME)

    val adapter = new SimpleCursorAdapter(this, R.layout.contact_entry, cursor,
      fields, Array(R.id.contactEntryText))
    mContactList setAdapter adapter
  }

  /**
   * Obtains the contact list for the currently selected account.
   *
   * @return A cursor for for accessing the contact list.
   */
  private def getContacts(): Cursor = {
    // Run query
    val uri = ContactsContract.Contacts.CONTENT_URI
    val projection = Array(
      ContactsContract2.Contacts._ID,
      ContactsContract2.Contacts.DISPLAY_NAME
    )
    val selection = ContactsContract2.Contacts.IN_VISIBLE_GROUP + " = '" +
                    (if (mShowInvisible) "0" else "1") + "'"
    var selectionArgs: Array[String] = Array()
    val sortOrder = ContactsContract2.Contacts.DISPLAY_NAME + " COLLATE LOCALIZED ASC"

    managedQuery(uri, projection, selection, selectionArgs, sortOrder)
  }

  /**
   * Launches the ContactAdder activity to add a new contact to the selected accont.
   */
  protected def launchContactAdder() {
    val i = new Intent(this, classOf[ContactAdder])
    startActivity(i)
  }
}
