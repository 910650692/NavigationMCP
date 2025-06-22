// INaviAutoStatusCallback.aidl
package com.sgm.navi.mapservice.common;

interface INaviAutoStatusCallback {
    void onNaviStatusChange(String status);

    void onPanelData(int panelDataStatus);

    void onNaviStartAfterFiveMinutes();

    void onNaviManualStop();

    void onNaviBroadcastStatus(boolean open);
}