package com.fy.navi.hmi.mapdata.search;

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

public class SearchMapDataModel extends BaseModel<SearchMapDataViewModel> implements MapDataCallBack {
    private static final String TAG = SearchMapDataModel.class.getSimpleName();
    private final MapDataPackage mapDataPackage;

    public SearchMapDataModel() {
        mapDataPackage = MapDataPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mapDataPackage.registerCallBack("SearchMapDataModel",this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 通过搜索关键字获取行政区域adcode列表
     * @param strKey
     * @return 返回行政区域数据
     */
    public ArrayList<ProvDataInfo> searchAdCode(final String strKey) {
        return mapDataPackage.searchAdCode(strKey);
    }

    /**
     * 开始下载
     * @param adCodeList
     */
    public void startAllTask(final ArrayList<Integer> adCodeList) {
        mapDataPackage.startAllTask(adCodeList);
    }

    /**
     * 暂停下载
     * @param adCodeList
     */
    public void pauseAllTask(final ArrayList<Integer> adCodeList) {
        mapDataPackage.pauseAllTask(adCodeList);
    }

    /**
     * 删除数据包
     * @param adCodeList
     */
    public void deleteAllTask(final ArrayList<Integer> adCodeList) {
        mapDataPackage.deleteAllTask(adCodeList);
    }

    /**
     * 取消下载
     * @param adCodeList
     */
    public void cancelAllTask(final ArrayList<Integer> adCodeList) {
        mapDataPackage.cancelAllTask(adCodeList);
    }

    @Override
    public void onDownLoadStatus(final CityDataInfo cityDataInfo) {
        if (cityDataInfo != null){
            Logger.d(TAG, "onDownLoadStatus: cityDataInfo = "  + GsonUtils.toJson(cityDataInfo));
            mViewModel.onDownLoadStatus(cityDataInfo);
        }
    }

    @Override
    public void onMergedStatusInfo(final MergedStatusBean mergedStatusInfo) {
        Logger.d(TAG, "onMergedStatusInfo: " + GsonUtils.toJson(mergedStatusInfo));
    }

    @Override
    public void onErrorNotify(final int downLoadMode, final int dataType, final int id, final int errType, final String errMsg) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(TAG, "onErrorNotify: downLoadMode = "  + downLoadMode +
                    " dataType = " + dataType + " errType = " + errType + " errMsg = " + errMsg);
            // 删除异常城市数据
            mapDataPackage.deleteErrorData(id);
        });
    }

    @Override
    public void onDeleteErrorData(final int downLoadMode, final int dataType, final int id, final int opCode) {
        Logger.d(TAG, "onDeleteErrorData: downLoadMode = "  + downLoadMode +
                " dataType = " + dataType + " opCode = " + opCode);
    }

    @Override
    public void onRequestCheckSuccess(final int downLoadMode, final int dataType, final int opCode) {

    }

}
