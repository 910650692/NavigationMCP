package com.fy.navi.hmi.drivingrecord;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.adapter.user.account.AccountAdapterCallBack;
import com.fy.navi.service.define.user.account.AccountUserInfo;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.logicpaket.user.account.AccountCallBack;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackCallBack;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.ui.base.BaseModel;

/**
 * @Description
 * @Author fh
 * @date 2024/12/24
 */
public class DrivingRecordModel extends BaseModel<DrivingRecordViewModel> implements UserTrackCallBack, AccountCallBack {

    private static final String TAG = DrivingRecordModel.class.getName();
    private final UserTrackPackage userTrackPackage;
    private final AccountPackage accountPackage;

    public DrivingRecordModel() {
        userTrackPackage = UserTrackPackage.getInstance();
        accountPackage = AccountPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        userTrackPackage.registerCallBack(this);
        accountPackage.registerCallBack("DrivingRecordModel",  this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     *  获取总里程（单位：m）
     */
    public int getTotalDistance() {
        return userTrackPackage.getTotalDistance();
    }

    /**
     *   获取总时长（单位：秒）
     */
    public int getTotalDuration() {
        return userTrackPackage.getTotalDuration();
    }

    /**
     * 从sdk获取行程数据列表保存到本地
     */
    public void getDrivingRecordData() {
        userTrackPackage.getDrivingRecordData();
    }

    /**
     * 获取行程数据列表（默认导航历史）
     */
    public void getDrivingRecordDataList() {
        mViewModel.updateDrivingRecordData(userTrackPackage.getDrivingRecordDataList());
    }

    /**
     * 获取巡航历史-行程数据列表（巡航历史）
     * @return
     */
    public void getDrivingRecordCruiseDataList() {
        mViewModel.updateDrivingRecordData(userTrackPackage.getDrivingRecordCruiseDataList());
    }

    /**
     * 根据生成的GPS轨迹文件，构造行程数据Json串，上传到同步库
     * 写到同步库里面，点击立即同步同步库进行同步，先进行数据同步，再进行轨迹同步
     *
     * @param trailDriveDataId 参考 轨迹文件命名
     * @param data             行程数据Json串
     * @return
     */
    public int setBehaviorData(String trailDriveDataId, String data) {
        return userTrackPackage.setBehaviorData(trailDriveDataId, data);
    }

    /**
     *  根据ID删除行程信息
     */
    public int delBehaviorData(String id) {
        return userTrackPackage.delBehaviorData(id);
    }

    /**
     *  获取指定id文件路径，用户数据同步
     */
    public String getFilePath(String behaviorDataById) {
        DrivingRecordDataBean dataBean = GsonUtils.convertToT(behaviorDataById, DrivingRecordDataBean.class);
        String id = dataBean.getId();
        return userTrackPackage.getFilePath(id);
    }

    /**
     * 判断当前账号是否已登录
     */
    public void isLogin() {
        mViewModel.updateLoginTipView(accountPackage.isLogin());
    }

    @Override
    public void notify(int eventType, int exCode) {
        // 同步事件回调
        Logger.d(TAG, "notify: eventType = " + eventType + " exCode = " + exCode);
        mViewModel.updateDrivingRecordData();
    }

    @Override
    public void notifyQRCodeLoginConfirm(int errCode, int taskId, AccountUserInfo result) {
        if (result != null && result.code == 1) {
            if (result.profileInfo != null) {
                ThreadManager.getInstance().postUi(() -> {
                    mViewModel.updateLoginTipView(true);
                });
            }
        }
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
