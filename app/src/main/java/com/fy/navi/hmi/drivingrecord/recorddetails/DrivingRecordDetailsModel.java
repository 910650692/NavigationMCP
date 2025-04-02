package com.fy.navi.hmi.drivingrecord.recorddetails;

import com.android.utils.log.Logger;

import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackCallBack;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.ui.base.BaseModel;


public class DrivingRecordDetailsModel extends BaseModel<DrivingRecordDetailsViewModel> implements UserTrackCallBack {

    private static final String TAG = DrivingRecordDetailsModel.class.getName();
    private final UserTrackPackage mUserTrackPackage;
    private final HistoryManager mHistoryManager;


    public DrivingRecordDetailsModel() {
        mUserTrackPackage = UserTrackPackage.getInstance();
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mUserTrackPackage.registerCallBack("DrivingRecordDetailsModel",this);
    }

    /**
     * 根据ID删除行程信息
     * @param id 行程ID
     */
    public void delBehaviorData(final String id) {
        mUserTrackPackage.delBehaviorData(id);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public void notify(final int eventType, final int exCode) {
        // 同步事件回调
        Logger.d(TAG, "notify: eventType = " + eventType + " exCode = " + exCode);
//        mViewModel.closeDrivingRecordDetailsView(); //删除数据后 关闭该页面并刷新行程主页
    }

    /**
     * 通过数据type删除其对应info
     * @param fileName 数据文件名
     */
    public void deleteValueByFileName(final String fileName) {
        mHistoryManager.deleteValueByFileName(fileName);
    }

    @Override
    public void onStartGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName) {

    }

    @Override
    public void onCloseGpsTrack(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {

    }
    @Override
    public void onGpsTrackDepInfo(final int n32SuccessTag, final String psSavePath, final String psFileName, final GpsTrackDepthBean depInfo) {

    }

}
