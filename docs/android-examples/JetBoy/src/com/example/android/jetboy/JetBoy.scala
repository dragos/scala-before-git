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

// Android JET demonstration code:
// See the JetBoyView.java file for examples on the use of the JetPlayer class.

package com.example.android.jetboy

import _root_.android.app.Activity
import _root_.android.os.Bundle
import _root_.android.util.Log
import _root_.android.view.{KeyEvent, View}
import _root_.android.widget.{Button, TextView}

class JetBoy extends Activity with View.OnClickListener {

  /** A handle to the thread that's actually running the animation. */
  private var mJetBoyThread: JetBoyView#JetBoyThread = _

  /** A handle to the View in which the game is running. */
  private var mJetBoyView: JetBoyView = _

  // the play start button
  private var mButton: Button = _

  // used to hit retry
  private var mButtonRetry: Button = _

  // the window for instructions and such
  private var mTextView: TextView = _

  // game window timer
  private var mTimerView: TextView = _

  /**
   * Required method from parent class
   * 
   * @param savedInstanceState - The previous instance of this app
   */
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)

    setContentView(R.layout.main)

    // get handles to the JetView from XML and the JET thread.
    mJetBoyView = findViewById(R.id.JetBoyView).asInstanceOf[JetBoyView]
    mJetBoyThread = mJetBoyView.getThread

    // look up the happy shiny button
    mButton = findViewById(R.id.Button01).asInstanceOf[Button]
    mButton setOnClickListener this

    mButtonRetry = findViewById(R.id.Button02).asInstanceOf[Button]
    mButtonRetry setOnClickListener this

    // set up handles for instruction text and game timer text
    mTextView = findViewById(R.id.text).asInstanceOf[TextView]
    mTimerView = findViewById(R.id.timer).asInstanceOf[TextView]

    mJetBoyView setTimerView mTimerView

    mJetBoyView setButtonView mButtonRetry

    mJetBoyView setTextView mTextView
  }

  /**
   * Handles component interaction
   * 
   * @param v The object which has been clicked
   */
  def onClick(v: View) {
    // this is the first screen
    if (mJetBoyThread.getGameState == mJetBoyView.JetBoyThread.STATE_START) {
      mButton setText "PLAY!"
      mTextView setVisibility View.VISIBLE

      mTextView setText R.string.helpText
      mJetBoyThread setGameState mJetBoyView.JetBoyThread.STATE_PLAY

    }
    // we have entered game play, now we about to start running
    else if (mJetBoyThread.getGameState == mJetBoyView.JetBoyThread.STATE_PLAY) {
      mButton setVisibility View.INVISIBLE
      mTextView setVisibility View.INVISIBLE
      mTimerView setVisibility View.VISIBLE
      mJetBoyThread setGameState mJetBoyView.JetBoyThread.STATE_RUNNING

    }
    // this is a retry button
    else if (mButtonRetry equals v) {
      mTextView setText R.string.helpText

      mButton setText "PLAY!"
      mButtonRetry setVisibility View.INVISIBLE
      // mButtonRestart setVisibility View.INVISIBLE

      mTextView setVisibility View.VISIBLE
      mButton setText "PLAY!"
      mButton setVisibility View.VISIBLE

      mJetBoyThread setGameState mJetBoyView.JetBoyThread.STATE_PLAY

    } else {
      Log.d("JB VIEW", "unknown click " + v.getId)

      Log.d("JB VIEW", "state is  " + mJetBoyThread.mState)

    }
  }

  /**
   * Standard override to get key-press events.
   */
  override def onKeyDown(keyCode: Int, msg: KeyEvent): Boolean =
    if (keyCode == KeyEvent.KEYCODE_BACK)
      super.onKeyDown(keyCode, msg)
    else
      mJetBoyThread.doKeyDown(keyCode, msg)

  /**
   * Standard override for key-up.
   */
  override def onKeyUp(keyCode: Int, msg: KeyEvent): Boolean =
    if (keyCode == KeyEvent.KEYCODE_BACK)
      super.onKeyUp(keyCode, msg)
    else
      mJetBoyThread.doKeyUp(keyCode, msg)

}
