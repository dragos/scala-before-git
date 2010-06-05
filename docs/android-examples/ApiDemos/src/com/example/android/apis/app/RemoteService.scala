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

import _root_.android.app.{Notification, NotificationManager, PendingIntent, Service}
import _root_.android.content.Intent
import _root_.android.content.Context._
import _root_.android.os.{RemoteException, Handler, IBinder, Message,
                          Process, RemoteCallbackList}
import _root_.android.widget.Toast

// Need the following import to get access to the app resources, since this
// class is in a sub-package.
import com.example.android.apis.R

/**
 * This is an example of implementing an application service that runs in a
 * different process than the application.  Because it can be in another
 * process, we must use IPC to interact with it.  The
 * {@link RemoteServiceController} and {@link RemoteServiceBinding} classes
 * show how to interact with the service.
 */
object RemoteService {
  private final val REPORT_MSG = 1
}

class RemoteService extends Service {
  import RemoteService._  // companion object

  /**
   * This is a list of callbacks that have been registered with the
   * service.  Note that this is package scoped (instead of private) so
   * that it can be accessed more efficiently from inner classes.
   */
  final val mCallbacks = new RemoteCallbackList[IRemoteServiceCallback]

  private var mValue = 0
  private var mNM: NotificationManager = _
    
  override def onCreate() {
    mNM = getSystemService(NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]

    // Display a notification about us starting.
    showNotification()
        
    // While this service is running, it will continually increment a
    // number.  Send the first message that is used to perform the
    // increment.
    mHandler sendEmptyMessage REPORT_MSG
  }

  override def onDestroy() {
    // Cancel the persistent notification.
    mNM.cancel(R.string.remote_service_started)

    // Tell the user we stopped.
    Toast.makeText(this, R.string.remote_service_stopped, Toast.LENGTH_SHORT).show()
        
    // Unregister all callbacks.
    mCallbacks.kill()
        
    // Remove the next pending message to increment the counter, stopping
    // the increment loop.
    mHandler removeMessages REPORT_MSG
  }

  override def onBind(intent: Intent): IBinder = {
    // Select the interface to return.  If your service only implements
    // a single interface, you can just return it here without checking
    // the Intent.
    if (classOf[IRemoteService].getName equals intent.getAction)
      mBinder
    else if (classOf[ISecondary].getName equals intent.getAction) 
      mSecondaryBinder
    else
      null
  }

  /**
   * The IRemoteInterface is defined through IDL
   */
  private val mBinder = new IRemoteService.Stub {
    def registerCallback(cb: IRemoteServiceCallback) {
      if (cb != null) mCallbacks.register(cb)
    }
    def unregisterCallback(cb: IRemoteServiceCallback) {
      if (cb != null) mCallbacks.unregister(cb)
    }
  }

  /**
   * A secondary interface to the service.
   */
  private val mSecondaryBinder = new ISecondary.Stub {
    def getPid(): Int = {
      Process.myPid()
    }
    def basicTypes(anInt: Int, aLong: Long, aBoolean: Boolean,
                   aFloat: Float, aDouble: Double, aString: String) {
    }
  }

  /**
   * Our Handler used to execute operations on the main thread.  This is used
   * to schedule increments of our value.
   */
  private final val mHandler = new Handler {
    override def handleMessage(msg: Message) {
      msg.what match {
                
        // It is time to bump the value!
        case REPORT_MSG =>
          // Up it goes.
          mValue += 1
          val value = mValue
                    
          // Broadcast to all clients the new value.
          val N = mCallbacks.beginBroadcast()
          for (i <- 0 until N) {
            try {
              mCallbacks.getBroadcastItem(i).valueChanged(value)
            } catch {
              case e: RemoteException =>
                // The RemoteCallbackList will take care of removing
                // the dead object for us.
            }
          }
          mCallbacks.finishBroadcast()
                    
          // Repeat every 1 second.
          sendMessageDelayed(obtainMessage(REPORT_MSG), 1*1000)

        case _ =>
          super.handleMessage(msg)
      }
    }
  }

  /**
   * Show a notification while this service is running.
   */
  private def showNotification() {
    // In this sample, we'll use the same text for the ticker and the
    // expanded notification
    val text = getText(R.string.remote_service_started)

    // Set the icon, scrolling text and timestamp
    val notification = new Notification(R.drawable.stat_sample, text,
                                        System.currentTimeMillis)

    // The PendingIntent to launch our activity if the user selects this notification
    val contentIntent = PendingIntent.getActivity(this, 0,
                new Intent(this, classOf[LocalServiceController]), 0)

    // Set the info for the views that show in the notification panel.
    notification.setLatestEventInfo(this, getText(R.string.remote_service_label),
                       text, contentIntent)

    // Send the notification.
    // We use a string id because it is a unique number.  We use it later to cancel.
    mNM.notify(R.string.remote_service_started, notification)
  }
}
