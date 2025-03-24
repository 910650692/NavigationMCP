package com.fy.navi.hmi.mapdata.manager;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.MergedStatusBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.logicpaket.mapdata.MapDataCallBack;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.ui.base.BaseModel;
import com.fy.navi.service.MapDefaultFinalTag;

import java.util.ArrayList;

public class ManagerMapDataModel extends BaseModel<ManagerMapDataViewModel> implements MapDataCallBack {
    private final MapDataPackage mMapDataPackage;

    public ManagerMapDataModel() {
        mMapDataPackage = MapDataPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mMapDataPackage.registerCallBack("ManagerMapDataModel",this);
    }

    /**
     * 初始化view
     */
    public void initView() {
        mViewModel.setDownloadingView(mMapDataPackage.getWorkingList());
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 获取下载中列表
     * @return 返回数据列表
     */
    public ArrayList<CityDataInfo> getWorkingList() {
        return mMapDataPackage.getWorkingList();
    }

    /**
     * 获取已下载列表
     * @return 返回已下载信息
     */
    public ArrayList<CityDataInfo> getWorkedList() {
        return mMapDataPackage.getWorkedList();
    }

    /**
     * 获取下载中、更新中、等待中等数据列表
     * @param cityDataInfos
     * @return 返回 数据信息
     */
    public ArrayList<Integer> getAllWorkingAdCodeList(final ArrayList<CityDataInfo> cityDataInfos) {
        return mMapDataPackage.getAllWorkingAdCodeList(cityDataInfos);
    }

    /**
     * 开始下载
     * @param adCodeList
     */
    public void startAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataPackage.startAllTask(adCodeList);
    }

    /**
     * 删除数据包
     * @param adCodeList
     */
    public void deleteAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataPackage.deleteAllTask(adCodeList);
    }

    /**
     * 暂停下载
     * @param adCodeList
     */
    public void pauseAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataPackage.pauseAllTask(adCodeList);
    }

    /**
     * 取消下载
     * @param adCodeList
     */
    public void cancelAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataPackage.cancelAllTask(adCodeList);
    }

    @Override
    public void onDownLoadStatus(final ProvDataInfo provDataInfo) {
        Logger.d(MapDefaultFinalTag.OFFLINE_HMI_TAG, "onDownLoadStatus: provDataInfo = " + GsonUtils.toJson(provDataInfo));
        if (provDataInfo != null && provDataInfo.getCityInfoList() != null && !provDataInfo.getCityInfoList().isEmpty()) {
            final ArrayList<CityDataInfo> cityDataInfos = getWorkingList();
            for (CityDataInfo cityDataInfo : cityDataInfos) {
                if (cityDataInfo.getAdcode() == provDataInfo.getCityInfoList().get(0).getAdcode()) {
                    cityDataInfo.setDownLoadInfo(provDataInfo.getCityInfoList().get(0).getDownLoadInfo());
                }
            }
            ThreadManager.getInstance().postUi(() -> {
                mViewModel.setDownloadingView(cityDataInfos);
            });
        }
    }

    @Override
    public void onMergedStatusInfo(final MergedStatusBean mergedStatusInfo) {
        Logger.d(MapDefaultFinalTag.OFFLINE_HMI_TAG, "onMergedStatusInfo: " + GsonUtils.toJson(mergedStatusInfo));
    }

    @Override
    public void onErrorNotify(final int downLoadMode, final int dataType, final int id, final int errType, final String errMsg) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(MapDefaultFinalTag.OFFLINE_HMI_TAG, "onErrorNotify: downLoadMode = "  + downLoadMode +
                    " dataType = " + dataType + " errType = " + errType + " errMsg = " + errMsg);
            // 删除异常城市数据
            mMapDataPackage.deleteErrorData(id);
        });
    }

    @Override
    public void onDeleteErrorData(final int downLoadMode, final int dataType,final int id, final int opCode) {
        Logger.d(MapDefaultFinalTag.OFFLINE_HMI_TAG, "onDeleteErrorData: downLoadMode = "  + downLoadMode +
                " dataType = " + dataType + " opCode = " + opCode);
    }

    @Override
    public void onRequestCheckSuccess(final int downLoadMode, final int dataType, final int opCode) {

    }

}
