// INaviAutoGuideStatusCallBack.aidl
package com.fy.navi.mapservice.common;

interface INaviAutoGuideStatusCallBack {

    void onNaviStartAfterFiveMinutes();

    void onNaviManualStop();
}