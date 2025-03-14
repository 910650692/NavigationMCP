package com.fy.navi.hmi.mapdata;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.MergedStatusBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.logicpaket.mapdata.MapDataCallBack;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/09
 */
public class MapDataModel extends BaseModel<MapDataViewModel> implements MapDataCallBack {

    private static final String TAG = MapDataModel.class.getName();
    private final MapDataPackage mapDataPackage;

    public MapDataModel() {
        mapDataPackage = MapDataPackage.getInstance();
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

    public CityDataInfo getCountryData() {
        return mapDataPackage.getCountryData();
    }

    /**
     * 获取当前城市信息
     * @param adCode
     * @return
     */
    public CityDataInfo getCurrentCityInfo(int adCode) {
        return mapDataPackage.getCityInfo(adCode);
    }

    /**
     * 获取下载中、更新中状态下的所有城市adCode列表
     * @return
     */
    public ArrayList<CityDataInfo> getWorkingList() {
        return mapDataPackage.getWorkingList();
    }

    /**
     * 获取已下载状态下的所有城市adCode列表
     * @return
     */
    public ArrayList<CityDataInfo> getWorkedList() {
        return mapDataPackage.getWorkedList();
    }

    /**
     * 获取 已下载状态的 省份+城市 结构 信息
     * @return
     */
    public ArrayList<ProvDataInfo> getAllDownLoadedList() {
        return mapDataPackage.getAllDownLoadedList();
    }

    /**
     * 通过adCode获取附近推荐城市信息
     * @param adCode
     * @return
     */
    public ArrayList<CityDataInfo> getNearAdCodeList(int adCode) {
        return mapDataPackage.getNearAdCodeList(adCode);
    }

    public void startAllTask(ArrayList<Integer> adCodeList) {
        mapDataPackage.startAllTask(adCodeList);
    }

    public void pauseAllTask(ArrayList<Integer> adCodeList) {
        mapDataPackage.pauseAllTask(adCodeList);
    }

    public void cancelAllTask(ArrayList<Integer> adCodeList) {
        mapDataPackage.cancelAllTask(adCodeList);
    }

    public void deleteAllTask(ArrayList<Integer> adCodeList) {
        mapDataPackage.deleteAllTask(adCodeList);
    }

    @Override
    public void onDownLoadStatus(ProvDataInfo provDataInfo) {
        mViewModel.onDownLoadStatus(provDataInfo);
    }

    @Override
    public void onPercent(ProvDataInfo info) {
        mViewModel.onPercent(info);
    }

    @Override
    public void onMergedStatusInfo(MergedStatusBean mergedStatusInfo) {
        Logger.d(TAG, "onMergedStatusInfo: " + GsonUtils.toJson(mergedStatusInfo));
    }

    @Override
    public void onErrorNotify(int downLoadMode, int dataType, int id, int errType, String errMsg) {
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
    public void onDeleteErrorData(int downLoadMode, int dataType, int id, int opCode) {
        Logger.d(TAG, "onDeleteErrorData: downLoadMode = "  + downLoadMode +  " dataType = "
                + dataType + " opCode = " + opCode);
    }
}
