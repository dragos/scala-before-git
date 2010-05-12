/*
 * Copyright (C) 2009 Google Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package com.example.android.livecubes.cube2

import com.example.android.livecubes.R

import _root_.android.content.SharedPreferences
import _root_.android.os.Bundle
import _root_.android.preference.PreferenceActivity

class CubeWallpaper2Settings extends PreferenceActivity
  with SharedPreferences.OnSharedPreferenceChangeListener {

  override protected def onCreate(icicle: Bundle) {
    super.onCreate(icicle)
    getPreferenceManager().setSharedPreferencesName(
            CubeWallpaper2.SHARED_PREFS_NAME)
    addPreferencesFromResource(R.xml.cube2_settings)
    getPreferenceManager().getSharedPreferences().registerOnSharedPreferenceChangeListener(
           this)
  }

  override protected def onResume() {
    super.onResume()
  }

  override protected def onDestroy() {
    getPreferenceManager().getSharedPreferences().unregisterOnSharedPreferenceChangeListener(
           this);
    super.onDestroy()
  }

  def onSharedPreferenceChanged(sharedPreferences: SharedPreferences,
            key: String) {
  }
}
