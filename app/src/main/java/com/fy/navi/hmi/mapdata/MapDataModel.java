package com.fy.navi.hmi.mapdata;

import android.text.TextUtils;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.MergedStatusBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.logicpaket.mapdata.MapDataCallBack;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.List;

public class MapDataModel extends BaseModel<MapDataViewModel> implements MapDataCallBack {

    private static final String TAG = MapDataModel.class.getName();
    private final MapDataPackage mapDataPackage;
    private CommonManager mCommonManager;

    public MapDataModel() {
        mapDataPackage = MapDataPackage.getInstance();
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mapDataPackage.registerCallBack("MapDataModel",this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public ArrayList<ProvDataInfo> getMapDataList() {
        return mapDataPackage.getMapDataList();
    }

    /**
     * 获取基础包信息
     * @return 返回基础包信息
     */
    public CityDataInfo getCountryData() {
        return mapDataPackage.getCountryData();
    }

    /**
     * 是否显示 下载基础功能包推荐弹窗（首次触发下载才显示）
     * @return false 不显示； true 显示
     */
    public boolean countryDataVisible() {
        final String value = mCommonManager.getValueByKey(UserDataCode.SETTING_DOWNLOAD_COUNTRY);
        return TextUtils.isEmpty(value);
    }

    /**
     * 获取当前城市信息
     * @return 返回当前城市信息
     */
    public CityDataInfo getCurrentCityInfo() {
        final LocInfoBean locationInfo = PositionPackage.getInstance().getLastCarLocation();
        int adCode = mapDataPackage.getAdCodeByLonLat(locationInfo.getLongitude(), locationInfo.getLatitude());
        return mapDataPackage.getCityInfo(adCode);
    }

    /**
     * 获取下载中、更新中状态下的所有城市adCode列表
     * @return 返回处于下载中、更新中的数据列表
     */
    public ArrayList<ProvDataInfo> getWorkingList() {
        return mapDataPackage.getWorkingList();
    }

    /**
     * 获取下载中、更新中状态下的所有城市adCode列表数量
     * @return int
     */
    public int getWorkingQueueSize() {
        ArrayList<Integer> adCodeList = mapDataPackage.getWorkingQueueAdCodeList();
        int size = 0;
        if (adCodeList != null && !adCodeList.isEmpty()) {
            size = adCodeList.size();
        }
        return size;
    }

    /**
     * 获取已下载状态下的所有城市adCode列表
     * @return 返回处于已下载的CityDataInfo数据列表
     */
    public ArrayList<ProvDataInfo> getWorkedList() {
        return mapDataPackage.getWorkedList();
    }

    /**
     * 获取 已下载状态的 省份+城市 结构 信息
     * @return 获取已经下载的ProvDataInfo数据列表
     */
    public ArrayList<ProvDataInfo> getAllDownLoadedList() {
        return mapDataPackage.getAllDownLoadedList();
    }

    /**
     * 通过adCode获取附近推荐城市信息
     * @return 返回附近城市的数据列表
     */
    public ArrayList<CityDataInfo> getNearAdCodeList() {
        final LocInfoBean locationInfo = PositionPackage.getInstance().getLastCarLocation();
        int adCode = mapDataPackage.getAdCodeByLonLat(locationInfo.getLongitude(), locationInfo.getLatitude());
        return mapDataPackage.getNearAdCodeList(adCode);
    }

    /**
     * 的数据包开始下载
     * @param adCodeList
     */
    public void startAllTask(final ArrayList<Integer> adCodeList) {
        mapDataPackage.startAllTask(adCodeList);
        mCommonManager.insertOrReplace(UserDataCode.SETTING_DOWNLOAD_COUNTRY, "首次触发下载");
    }

    /**
     * 暂停正在下载的数据包
     * @param adCodeList
     */
    public void pauseAllTask(final ArrayList<Integer> adCodeList) {
        mapDataPackage.pauseAllTask(adCodeList);
    }

    /**
     * 取消正在下载的数据包
     * @param adCodeList
     */
    public void cancelAllTask(final ArrayList<Integer> adCodeList) {
        mapDataPackage.cancelAllTask(adCodeList);
    }

    /**
     * 删除已下载的数据包
     * @param adCodeList
     */
    public void deleteAllTask(final ArrayList<Integer> adCodeList) {
        mapDataPackage.deleteAllTask(adCodeList);
    }

    /**
     * 发起云端数据列表检测
     * @param ischeck
     */
    public void requestDataListCheck(final boolean ischeck) {
        final String value = mCommonManager.getValueByKey(UserDataCode.SETTING_MESSAGE_CHECK_MAP_DATA_TIME);
        if (TextUtils.isEmpty(value)) { // 首次检测
            mapDataPackage.requestDataListCheck();
        } else {
            if (ischeck) { // 每15/45日检测一次
                mapDataPackage.requestDataListCheck();
            }
        }
    }

    @Override
    public void onDownLoadStatus(final CityDataInfo cityDataInfo) {
        if (cityDataInfo != null){
            mViewModel.onDownLoadStatus(cityDataInfo);
        }
    }

    @Override
    public void onMergedStatusInfo(final MergedStatusBean mergedStatusInfo) {
        Logger.d(TAG, "onMergedStatusInfo: " + GsonUtils.toJson(mergedStatusInfo));
    }

    @Override
    public void onErrorNotify(final int downLoadMode, final int dataType, final int id,
                              final int errType, final String errMsg) {
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                Logger.d(TAG, "onErrorNotify: downLoadMode = "  + downLoadMode +  " dataType = "
                        + dataType + " errType = " + errType + " errMsg = " + errMsg);
                // 删除异常城市数据
                mapDataPackage.deleteErrorData(id);
            }
        }, 0);

    }

    @Override
    public void onDeleteErrorData(final int downLoadMode, final int dataType,
                                  final int id, final int opCode) {
        Logger.d(TAG, "onDeleteErrorData: downLoadMode = "  + downLoadMode +  " dataType = "
                + dataType + " opCode = " + opCode);
    }

    @Override
    public void onRequestCheckSuccess(final int downLoadMode, final int dataType, final int opCode) {

    }
}
