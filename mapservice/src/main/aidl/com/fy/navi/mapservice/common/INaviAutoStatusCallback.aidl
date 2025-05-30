// INaviAutoStatusCallback.aidl
package com.fy.navi.mapservice.common;

interface INaviAutoStatusCallback {
    void onNaviStatusChange(String status);

    void onPanelData(int panelDataStatus);

    void onCountDownLightInfo(String lightInfo);
}