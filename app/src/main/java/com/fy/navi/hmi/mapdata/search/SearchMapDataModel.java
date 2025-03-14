package com.fy.navi.hmi.mapdata.search;

import static com.fy.navi.service.MapDefaultFinalTag.OFFLINE_HMI_TAG;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.mapdata.MergedStatusBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.logicpaket.mapdata.MapDataCallBack;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/02/17
 */
public class SearchMapDataModel extends BaseModel<SearchMapDataViewModel> implements MapDataCallBack {
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
     * @return
     */
    public ArrayList<ProvDataInfo> searchAdCode(String strKey) {
        return mapDataPackage.searchAdCode(strKey);
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

    @Override
    public void onDownLoadStatus(ProvDataInfo provDataInfo) {
        if (provDataInfo != null) {
            Logger.d(OFFLINE_HMI_TAG, "onDownLoadStatus: provDataInfo = "  + GsonUtils.toJson(provDataInfo));
        }
    }

    @Override
    public void onPercent(ProvDataInfo info) {
        if (info != null && info.downLoadInfo != null) {
            Logger.d(OFFLINE_HMI_TAG," percent = " + info.downLoadInfo.percent);
            mViewModel.onPercent(info);
        }
    }

    @Override
    public void onMergedStatusInfo(MergedStatusBean mergedStatusInfo) {
        Logger.d(OFFLINE_HMI_TAG, "onMergedStatusInfo: " + GsonUtils.toJson(mergedStatusInfo));
    }

    @Override
    public void onErrorNotify(int downLoadMode, int dataType, int id, int errType, String errMsg) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.d(OFFLINE_HMI_TAG, "onErrorNotify: downLoadMode = "  + downLoadMode +
                    " dataType = " + dataType + " errType = " + errType + " errMsg = " + errMsg);
            // 删除异常城市数据
            mapDataPackage.deleteErrorData(id);
        });
    }

    @Override
    public void onDeleteErrorData(int downLoadMode, int dataType, int id, int opCode) {
        Logger.d(OFFLINE_HMI_TAG, "onDeleteErrorData: downLoadMode = "  + downLoadMode +
                " dataType = " + dataType + " opCode = " + opCode);
    }

}
