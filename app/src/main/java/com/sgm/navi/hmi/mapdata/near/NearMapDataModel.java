package com.sgm.navi.hmi.mapdata.near;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.mapdata.MergedStatusBean;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.logicpaket.mapdata.MapDataCallBack;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.ui.base.BaseModel;
import com.sgm.navi.service.MapDefaultFinalTag;

import java.util.ArrayList;

public class NearMapDataModel extends BaseModel<NearMapDataViewModel> implements MapDataCallBack {
    private final MapDataPackage mMapDataPackage;

    public NearMapDataModel() {
        mMapDataPackage = MapDataPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mMapDataPackage.registerCallBack("NearMapDataModel",this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 初始化view
     */
    public void initData() {
        final LocInfoBean locationInfo = PositionPackage.getInstance().getLastCarLocation();
        final int adCode = mMapDataPackage.getAdCodeByLonLat(locationInfo.getLongitude(), locationInfo.getLatitude());
        mViewModel.setNearCityInfo(getNearAdCodeList(adCode));
    }

    /**
     * 通过code获取附件推荐城市
     * @param adCode
     * @return 返回推荐城市信息
     */
    public ArrayList<CityDataInfo> getNearAdCodeList(final int adCode) {
        return mMapDataPackage.getNearAdCodeList(adCode);
    }

    /**
     * 删除数据包
     * @param adCodeList
     */
    public void deleteAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataPackage.deleteAllTask(adCodeList);
    }

    /**
     * 下载数据包
     * @param adCodeList
     */
    public void startAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataPackage.startAllTask(adCodeList);
    }

    /**
     * 暂停下载数据包
     * @param adCodeList
     */
    public void pauseAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataPackage.pauseAllTask(adCodeList);
    }

    /**
     * 取消下载数据包
     * @param adCodeList
     */
    public void cancelAllTask(final ArrayList<Integer> adCodeList) {
        mMapDataPackage.cancelAllTask(adCodeList);
    }

    @Override
    public void onDownLoadStatus(final CityDataInfo cityDataInfo) {
        if (Logger.openLog) {
            Logger.d(MapDefaultFinalTag.OFFLINE_HMI_TAG, "onDownLoadStatus: cityDataInfo = " + cityDataInfo);
        }
        if (cityDataInfo != null && mViewModel != null) {
            mViewModel.onDownLoadStatus(cityDataInfo);
        }
    }

    @Override
    public void onMergedStatusInfo(final MergedStatusBean mergedStatusInfo) {
        if (Logger.openLog) {
            Logger.d(MapDefaultFinalTag.OFFLINE_HMI_TAG, "onMergedStatusInfo: ", mergedStatusInfo);
        }
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
    public void onDeleteErrorData(final int downLoadMode, final int dataType, final int id, final int opCode) {
        Logger.d(MapDefaultFinalTag.OFFLINE_HMI_TAG, "onDeleteErrorData: downLoadMode = "  + downLoadMode +
                " dataType = " + dataType + " opCode = " + opCode);
    }

    @Override
    public void onRequestCheckSuccess(final int downLoadMode, final int dataType, final int opCode) {

    }

}
