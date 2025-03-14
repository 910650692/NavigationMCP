package com.fy.navi.hmi.drivingrecord.recorddetails;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackCallBack;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.ui.base.BaseModel;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/24
 */
public class DrivingRecordDetailsModel extends BaseModel<DrivingRecordDetailsViewModel> implements UserTrackCallBack {

    private static final String TAG = DrivingRecordDetailsModel.class.getName();
    private final UserTrackPackage userTrackPackage;

    public DrivingRecordDetailsModel() {
        userTrackPackage = UserTrackPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        userTrackPackage.registerCallBack(this);
    }

    /**
     * 根据ID删除行程信息
     */
    public void delBehaviorData(String id) {
        userTrackPackage.delBehaviorData(id);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public void notify(int eventType, int exCode) {
        // 同步事件回调
        Logger.d(TAG, "notify: eventType = " + eventType + " exCode = " + exCode);
//        mViewModel.closeDrivingRecordDetailsView(); //删除数据后 关闭该页面并刷新行程主页
    }

    @Override
    public void onStartGpsTrack(int n32SuccessTag, String psSavePath, String psFileName) {

    }

    @Override
    public void onCloseGpsTrack(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthBean depInfo) {

    }

    @Override
    public void onGpsTrackDepInfo(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthBean depInfo) {

    }

}
