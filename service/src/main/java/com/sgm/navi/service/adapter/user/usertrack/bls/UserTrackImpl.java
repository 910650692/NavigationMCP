package com.sgm.navi.service.adapter.user.usertrack.bls;


import com.android.utils.log.Logger;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.model.BehaviorDataType;
import com.autonavi.gbl.user.syncsdk.model.SyncMode;
import com.autonavi.gbl.user.usertrack.UserTrackService;
import com.autonavi.gbl.user.usertrack.model.BehaviorDurationType;
import com.autonavi.gbl.user.usertrack.model.BehaviorFileType;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.user.usertrack.IUserTrackApi;
import com.sgm.navi.service.adapter.user.usertrack.UserTrackAdapterCallBack;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.sgm.navi.service.define.user.usertrack.HistoryRouteItemBean;
import com.sgm.navi.service.define.user.usertrack.SearchHistoryItemBean;

import java.util.ArrayList;

public class UserTrackImpl implements IUserTrackApi {
    private static final String TAG = MapDefaultFinalTag.USER_TRACK_SERVICE_TAG;
    private UserTrackImplHelper mAdapterImplHelper;
    private UserTrackService mUserTrackService;

    public UserTrackImpl() {

    }

    @Override
    public void initUserTrackService() {
        if(null == mUserTrackService)
            mUserTrackService = (UserTrackService) ServiceMgr.getServiceMgrInstance()
                    .getBLService(SingleServiceID.UserTrackSingleServiceID);
        mAdapterImplHelper = new UserTrackImplHelper(mUserTrackService);
        mAdapterImplHelper.initUserTrackService();
    }

    @Override
    public void registerCallBack(final String key, final UserTrackAdapterCallBack callBack) {
        mAdapterImplHelper.registerCallBack(key, callBack);
    }

    @Override
    public void unRegisterCallback(final String key) {
        mAdapterImplHelper.unRegisterCallBack(key);
    }

    @Override
    public void unInitUserTrackService() {
        mAdapterImplHelper.unInitUserTrackService();
    }

    /**
     * 获取初始化状态
     * @return 初始化状态
     */
    @Override
    public int isInit() {
        if (mUserTrackService == null) {
            return -1;
        }
        return mUserTrackService.isInit();
    }

    /**
     * 添加搜索历史记录
     * @return 添加结果
     */
    @Override
    public int addSearchHistory(final SearchHistoryItemBean bean) {
       return mAdapterImplHelper.addSearchHistory(bean);
    }

    /**
     * 获取搜索历史记录列表
     * @return 搜索历史记录列表
     */
    @Override
    public ArrayList<SearchHistoryItemBean> getSearchHistory() {
        return mAdapterImplHelper.getSearchHistory();
    }

    /**
     * 删除所有搜索历史记录
     * @return 删除结果
     */
    @Override
    public int clearSearchHistory() {
        if (mUserTrackService == null) {
            return -1;
        }
        Logger.d(TAG, "clearSearchHistory ");
        return mUserTrackService.clearSearchHistory(SyncMode.SyncModeNow);
    }

    @Override
    public ArrayList<HistoryRouteItemBean> getHistoryRoute() {
        if (mUserTrackService == null) {
            return null;
        }
        return mAdapterImplHelper.getHistoryRoute();
    }

    @Override
    public int addHistoryRoute(final HistoryRouteItemBean item) {
        if (mUserTrackService == null) {
            return -1;
        }
        return mAdapterImplHelper.addHistoryRoute(item);
    }

    @Override
    public int delHistoryRoute(final HistoryRouteItemBean bean) {
        if (mUserTrackService == null) {
            return -1;
        }
        return mAdapterImplHelper.delHistoryRoute(bean);
    }

    @Override
    public int clearHistoryRoute() {
        if (mUserTrackService == null) {
            return -1;
        }
        Logger.d(TAG, "clearHistoryRoute ");
        return mAdapterImplHelper.clearHistoryRoute();
    }

    /**
     * 删除搜索历史记录, 删除只需要赋值名字字段
     * @param name 搜索历史记录名称
     * @return 删除结果
     */
    @Override
    public int delSearchHistory(final String name) {
        return mAdapterImplHelper.delSearchHistory(name);
    }

    /**
     * 启动Gps打点，并生成轨迹文件
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @param un32MsecRate 打点频率（单位毫秒，值不低于500），如传入5000表示，每5000毫秒打一个GPS点
     * @return 结果值
     */
    @Override
    public int startGpsTrack(final String psSavePath, final String psFileName, final long un32MsecRate) {
        if (mUserTrackService == null) {
            return -1;
        }
        return mUserTrackService.startGpsTrack(psSavePath, psFileName, un32MsecRate);
    }

    /**
     * 关闭Gps打点，并生成轨迹文件
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    @Override
    public int closeGpsTrack(final String psSavePath, final String psFileName) {
        if (mUserTrackService == null) {
            return -1;
        }
        return mUserTrackService.closeGpsTrack(psSavePath, psFileName);
    }

    /**
     * 获取指定轨迹文件的深度信息，通过异步回调返回。
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    @Override
    public int obtainGpsTrackDepInfo(final String psSavePath, final String psFileName) {
        if (mUserTrackService == null) {
            return -1;
        }
        Logger.d(TAG, "obtainGpsTrackDepInfo psSavePath:" , psSavePath , " psFileName:" , psFileName);
        return mUserTrackService.obtainGpsTrackDepInfo(psSavePath, psFileName);
    }

    /**
     * 设置行程信息
     * 写到同步库里面，点击立即同步同步库进行同步，先进行数据同步，再进行轨迹同步
     * @param id 行程ID
     * @param data 行程信息
     * @return 设置结果
     */
    @Override
    public int setBehaviorData(final String id, final String data) {
        if (mUserTrackService == null) {
            return -1;
        }
        return mUserTrackService.setBehaviorData(BehaviorDataType.BehaviorTypeTrailDriveForAuto, id, data, SyncMode.SyncModeNow);
    }

    /**
     * 获取行为数据
     * @param type 行为数据类型
     * @param id 行为数据ID
     * @return 行为数据
     */
    @Override
    public String getBehaviorData(final int type, final String id) {
        if (mUserTrackService == null) {
            return "";
        }
        return mUserTrackService.getBehaviorData(type, id);
    }

    /**
     * 清空行为数据
     * @param type 行为数据类型
     * @param mode 同步方式
     * @return 清空结果
     */
    @Override
    public int clearBehaviorData(final int type, final int mode) {
        if (mUserTrackService == null) {
            return -1;
        }
        return mUserTrackService.clearBehaviorData(type, mode);
    }

    /**
     * 根据ID删除行程信息
     * @param id 行程ID
     * @return 删除结果
     */
    @Override
    public int delBehaviorData(final String id) {
        if (mUserTrackService == null) {
            return -1;
        }
        final int ret = mUserTrackService.delBehaviorData(BehaviorDataType.BehaviorTypeTrailDriveForAuto,
                id, SyncMode.SyncModeNow);
        Logger.i(TAG, "delBehaviorData  ret: " + ret);
        return ret;
    }

    /**
     * 获取行为数据id列表
     * @return 行为数据id列表
     */
    public int[] getBehaviorDataIds() {
        final int type = BehaviorDataType.BehaviorTypeTrailDriveForAuto;
        return mUserTrackService.getBehaviorDataIds(type);
    }

    /**
     * 从sdk获取行程数据列表
     */
    @Override
    public void getDrivingRecordData() {
        mAdapterImplHelper.getDrivingRecordData();
    }

    /**
     * 获取行程数据列表
     * @return 行程数据列表
     */
    @Override
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        return mAdapterImplHelper.getDrivingRecordDataList();
    }



    /**
     * 获取巡航历史-行程数据列表（巡航历史）
     * @return 行程数据列表
     */
    @Override
    public ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList() {
        return mAdapterImplHelper.getDrivingRecordCruiseDataList();
    }

    /**
     * 从sdk获取当前用户行程数据列表（默认导航历史）
     * @return 行程数据列表
     */
    @Override
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataFromSdk() {
        return mAdapterImplHelper.getDrivingRecordDataFromSdk();
    }

    /**
     * 获取总时长（单位：秒）
     * @return 总时长
     */
    @Override
    public int getTotalDuration() {
        if (mUserTrackService == null) {
            return -1;
        }
        final int ret = mUserTrackService.getTotalDuration(BehaviorDataType.BehaviorTypeTrailDriveForAuto);
        Logger.i(TAG, "getTotalDuration  ret: " + ret);
        return ret;
    }

    /**
     *  获取总里程（单位：m）
     * @return 总里程
     */
    @Override
    public int getTotalDistance() {
        if (mUserTrackService == null) {
            return -1;
        }
        final int ret = mUserTrackService.getTotalDistance(BehaviorDataType.BehaviorTypeTrailDriveForAuto,
                BehaviorDurationType.BehaviorDurationWhole);
        Logger.i(TAG, "getTotalDistance  ret: " + ret);
        return ret;
    }

    /**
     * 获取置顶id文件路径，用户同步数据
     * @param id 行程ID
     * @return 文件路径
     */
    @Override
    public String getFilePath(final String id) {
        if (mUserTrackService == null) {
            return "";
        }
        return mUserTrackService.getFilePath(BehaviorDataType.BehaviorTypeTrailDriveForAuto, id, BehaviorFileType.BehaviorFileTrail);
    }
}
