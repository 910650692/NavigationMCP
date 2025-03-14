package com.fy.navi.service.adapter.user.usertrack;

import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;

/**
 * @Description
 * @Author fh
 * @date 2024/12/26
 */
public interface UserTrackAdapterCallBack {
    void onStartGpsTrack(int n32SuccessTag, String psSavePath, String psFileName);

    void onCloseGpsTrack(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthBean depInfo);

    void onGpsTrackDepInfo(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthBean depInfo);

    void notify(int eventType,  int exCode);
}
