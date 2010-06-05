/*
 * Copyright (C) 2007 The Android Open Source Project
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

package com.example.android.apis.app

// Need the following import to get access to the app resources, since this
// class is in a sub-package.
import com.example.android.apis.R

import _root_.android.app.{Activity, NotificationManager}
import _root_.android.content.Intent
import _root_.android.content.Context._
import _root_.android.os.Bundle
import _root_.android.view.{Gravity, View, ViewGroup, WindowManager}
import _root_.android.widget.{ImageButton, LinearLayout, RelativeLayout}


/**
 * Activity used by StatusBarNotification to show the notification to the user.
 */
class NotificationDisplay extends Activity with View.OnClickListener {
  /**
   * Initialization of the Activity after it is first created.  Must at least
   * call {@link android.app.Activity#setContentView setContentView()} to
   * describe what is to be displayed in the screen.
   */
  override protected def onCreate(icicle: Bundle) {
    // Be sure to call the super class.
    super.onCreate(icicle)

    // Have the system blur any windows behind this one.
    getWindow.setFlags(WindowManager.LayoutParams.FLAG_BLUR_BEHIND,
                       WindowManager.LayoutParams.FLAG_BLUR_BEHIND)
        
    val container = new RelativeLayout(this)
        
    val button = new ImageButton(this)
    button setImageResource getIntent.getIntExtra("moodimg", 0)
    button setOnClickListener this
        
    val lp = new RelativeLayout.LayoutParams(
               /*RelativeLayout*/ViewGroup.LayoutParams.WRAP_CONTENT,
               /*RelativeLayout*/ViewGroup.LayoutParams.WRAP_CONTENT)
    lp.addRule(RelativeLayout.CENTER_IN_PARENT)
        
    container.addView(button, lp)
        
    setContentView(container)
  }

  def onClick(v: View) {
    // The user has confirmed this notification, so remove it.
    getSystemService(NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]
      .cancel(R.layout.status_bar_notifications)
        
    // Pressing on the button brings the user back to our mood ring,
    // as part of the api demos app.  Note the use of NEW_TASK here,
    // since the notification display activity is run as a separate task.
    val intent = new Intent(this, classOf[StatusBarNotifications])
    intent setAction Intent.ACTION_MAIN
    intent setFlags Intent.FLAG_ACTIVITY_NEW_TASK
    startActivity(intent)
        
    // We're done.
    finish()
  }
}
