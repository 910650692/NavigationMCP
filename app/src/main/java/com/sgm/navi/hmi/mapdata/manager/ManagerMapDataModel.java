package com.sgm.navi.hmi.mapdata.manager;

import android.text.TextUtils;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.mapdata.MergedStatusBean;
import com.sgm.navi.service.define.mapdata.ProvDataInfo;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.logicpaket.mapdata.MapDataCallBack;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.ui.base.BaseModel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ManagerMapDataModel extends BaseModel<ManagerMapDataViewModel> implements MapDataCallBack {
    private final MapDataPackage mMapDataPackage;
    private CommonManager mCommonManager;

    public ManagerMapDataModel() {
        mMapDataPackage = MapDataPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mMapDataPackage.registerCallBack("ManagerMapDataModel",this);
        mCommonManager = CommonManager.getInstance();
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
    public ArrayList<ProvDataInfo> getWorkingList() {
        return mMapDataPackage.getWorkingList();
    }

    /**
     * 获取已下载列表
     * @return 返回已下载信息
     */
    public  ArrayList<ProvDataInfo>  getWorkedList() {
        return mMapDataPackage.getWorkedList();
    }

    /**
     * 获取下载中、更新中、等待中等数据列表
     * @param cityDataInfos
     * @return 返回 数据信息
     */
    public ArrayList<Integer> getAllWorkingAdCodeList(final ArrayList<ProvDataInfo> cityDataInfos) {
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
    public void onDownLoadStatus(final CityDataInfo cityDataInfo) {
        if (cityDataInfo == null) {
            Logger.d(MapDefaultFinalTag.OFFLINE_HMI_TAG, "onDownLoadStatus: cityDataInfo is null");
            return;
        }
        Logger.d(MapDefaultFinalTag.OFFLINE_HMI_TAG, "onDownLoadStatus: mViewModel = ",
                (mViewModel == null), "; cityDataInfo = ", cityDataInfo.getAdcode(),
                cityDataInfo.getAreaType(), cityDataInfo.getName());
        if (mViewModel != null) {
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
    public void onDeleteErrorData(final int downLoadMode, final int dataType,final int id, final int opCode) {
        Logger.d(MapDefaultFinalTag.OFFLINE_HMI_TAG, "onDeleteErrorData: downLoadMode = "  + downLoadMode +
                " dataType = " + dataType + " opCode = " + opCode);
    }

    @Override
    public void onRequestCheckSuccess(final int downLoadMode, final int dataType, final int opCode) {

    }

    /**
     * 保存已下载城市code
     * @param value
     */
    public void saveCachedCityList(String value) {
        mCommonManager.insertOrReplace(UserDataCode.MAP_DATA_DOWNLOADED_CITY_LIST, value);
    }

    /**
     * 获取已下载城市code列表
     * @return list
     */
    public List<String> getCachedCityList() {
        List<String> list = new ArrayList<>() ;
        String value = mCommonManager.getValueByKey(UserDataCode.MAP_DATA_DOWNLOADED_CITY_LIST);
        if (!TextUtils.isEmpty(value)) {
            String[] split = value.split(",");
            list = Arrays.asList(split);
        }
        return list;
    }

}
