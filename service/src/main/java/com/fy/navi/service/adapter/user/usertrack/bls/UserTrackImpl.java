package com.fy.navi.service.adapter.user.usertrack.bls;


import com.android.utils.log.Logger;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.model.BehaviorDataType;
import com.autonavi.gbl.user.syncsdk.model.SyncMode;
import com.autonavi.gbl.user.usertrack.UserTrackService;
import com.autonavi.gbl.user.usertrack.model.BehaviorDurationType;
import com.autonavi.gbl.user.usertrack.model.BehaviorFileType;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.usertrack.IUserTrackApi;
import com.fy.navi.service.adapter.user.usertrack.UserTrackAdapterCallBack;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.define.user.usertrack.SearchHistoryItemBean;

import java.util.ArrayList;

/**
 * 高德行为数据服务. 包含 搜索历史、用户行驶里程以及GPS轨迹生成功能。
 *
 * @Description Impl类只做SDK的原子能力封装，不做对象及数据转换
 * @Author fh
 * @date 2024/12/26
 */
public class UserTrackImpl implements IUserTrackApi {
    private static final String TAG = MapDefaultFinalTag.USER_TRACK_SERVICE_TAG;
    private UserTrackImplHelper adapterImplHelper;
    private UserTrackService mUserTrackService;

    public UserTrackImpl() {
        mUserTrackService = (UserTrackService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.UserTrackSingleServiceID);
        adapterImplHelper = new UserTrackImplHelper(mUserTrackService);
    }

    @Override
    public void initUserTrackService() {
        adapterImplHelper.initUserTrackService();
    }

    @Override
    public void registerCallBack(String key, UserTrackAdapterCallBack callBack) {
        adapterImplHelper.registerCallBack(key, callBack);
    }

    @Override
    public void unRegisterCallback(String key) {
        adapterImplHelper.unRegisterCallBack(key);
    }

    @Override
    public void unInitUserTrackService() {
        adapterImplHelper.unInitUserTrackService();
    }

    /**
     * 获取初始化状态
     * @return
     */
    @Override
    public int isInit() {
        if (mUserTrackService == null) return -1;
        return mUserTrackService.isInit();
    }

    /**
     * 添加搜索历史记录
     * @return
     */
    @Override
    public int addSearchHistory(SearchHistoryItemBean bean) {
       return adapterImplHelper.addSearchHistory(bean);
    }

    /**
     * 获取搜索历史记录列表
     * @return
     */
    @Override
    public ArrayList<SearchHistoryItemBean> getSearchHistory() {
        return adapterImplHelper.getSearchHistory();
    }

    /**
     * 删除所有搜索历史记录
     * @param mode
     * @return
     */
    @Override
    public int clearSearchHistory(int mode) {
        if (mUserTrackService == null) return -1;
        return mUserTrackService.clearSearchHistory(mode);
    }

    /**
     * 删除指定搜索历史记录
     * @param name
     * @return
     */
    @Override
    public int delSearchHistory(String name) {
        return adapterImplHelper.delSearchHistory(name);
    }

    /**
     * 开始打点，生成轨迹文件
     * @param psSavePath
     * @param psFileName
     * @param un32MsecRate
     * @return
     */
    @Override
    public int startGpsTrack(String psSavePath, String psFileName, long un32MsecRate) {
        if (mUserTrackService == null) return -1;
        return mUserTrackService.startGpsTrack(psSavePath, psFileName, un32MsecRate);
    }

    /**
     *  停止打点，调用时机 停止导航、巡航导航切换
     * @param psSavePath
     * @param psFileName
     * @return
     */
    @Override
    public int closeGpsTrack(String psSavePath, String psFileName) {
        if (mUserTrackService == null) return -1;
        return mUserTrackService.closeGpsTrack(psSavePath, psFileName);
    }

    /**
     * 获取轨迹文件信息
     * @param psSavePath
     * @param psFileName
     * @return
     */
    @Override
    public int obtainGpsTrackDepInfo(String psSavePath, String psFileName) {
        if (mUserTrackService == null) return -1;
        return mUserTrackService.obtainGpsTrackDepInfo(psSavePath, psFileName);
    }

    /**
     * 设置行为数据
     * @param id
     * @param data
     * @return
     */
    @Override
    public int setBehaviorData(String id, String data) {
        if (mUserTrackService == null) return -1;
        return mUserTrackService.setBehaviorData(BehaviorDataType.BehaviorTypeTrailDriveForAuto, id, data, SyncMode.SyncModeNow);
    }

    /**
     * 获取行为数据
     * @param type
     * @param id
     * @return
     */
    @Override
    public String getBehaviorData(int type, String id) {
        if (mUserTrackService == null) return "";
        return mUserTrackService.getBehaviorData(type, id);
    }

    /**
     * 删除行为数据
     * @param type
     * @param mode
     * @return
     */
    @Override
    public int clearBehaviorData(int type, int mode) {
        if (mUserTrackService == null) return -1;
        return mUserTrackService.clearBehaviorData(type, mode);
    }

    /**
     * 根据ID删除行程信息
     * @param id
     * @return
     */
    @Override
    public int delBehaviorData(String id) {
        if (mUserTrackService == null) return -1;
        int ret = mUserTrackService.delBehaviorData(BehaviorDataType.BehaviorTypeTrailDriveForAuto,
                id, SyncMode.SyncModeNow);
        Logger.i(TAG, "delBehaviorData  ret: " + ret);
        return ret;
    }

    /**
     * 从sdk获取行程数据列表
     * @return
     */
    @Override
    public void getDrivingRecordData() {
        adapterImplHelper.getDrivingRecordData();
    }

    /**
     * 获取行程数据列表
     * @return
     */
    @Override
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        return adapterImplHelper.getDrivingRecordDataList();
    }

    /**
     * 获取巡航历史-行程数据列表（巡航历史）
     * @return
     */
    @Override
    public ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList() {
        return adapterImplHelper.getDrivingRecordCruiseDataList();
    }

    /**
     * 获取总时长（单位：秒）
     * @return
     */
    @Override
    public int getTotalDuration() {
        if (mUserTrackService == null) return -1;
        int ret = mUserTrackService.getTotalDuration(BehaviorDataType.BehaviorTypeTrailDriveForAuto);
        Logger.i(TAG, "getTotalDuration  ret: " + ret);
        return ret;
    }

    /**
     *  获取总里程（单位：m）
     * @return
     */
    @Override
    public int getTotalDistance() {
        if (mUserTrackService == null) return -1;
        int ret = mUserTrackService.getTotalDistance(BehaviorDataType.BehaviorTypeTrailDriveForAuto,
                BehaviorDurationType.BehaviorDurationWhole);
        Logger.i(TAG, "getTotalDistance  ret: " + ret);
        return ret;
    }

    /**
     * 获取指定id文件路径 获取同步库轨迹文件
     * @param id
     * @return
     */
    @Override
    public String getFilePath(String id) {
        if (mUserTrackService == null) return "";
        return mUserTrackService.getFilePath(BehaviorDataType.BehaviorTypeTrailDriveForAuto, id, BehaviorFileType.BehaviorFileTrail);
    }
}
