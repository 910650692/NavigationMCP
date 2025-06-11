// INaviAutoSpeedCallBack.aidl
package com.fy.navi.mapservice.common;

// Declare any non-default types here with import statements

interface INaviAutoSpeedCallBack {

     void onSpeedLimitChange(int curSpeed, int limitSpeed);

}
