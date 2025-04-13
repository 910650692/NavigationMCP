package com.fy.navi.service.adapter.mapdata;

import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.MergedStatusBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

public interface MapDataAdapterCallBack {
    /**
     * 下载状态回调
     * @param cityDataInfo
     */
    void onDownLoadStatus(CityDataInfo cityDataInfo );

    /**
     * 引擎数据融合状态信息回调
     * @param mergedStatusInfo
     */
    void onMergedStatusInfo(MergedStatusBean mergedStatusInfo);

    /**
     * 数据异常通知
     * @param downLoadMode
     * @param dataType
     * @param id
     * @param errType
     * @param errMsg
     */
    void onErrorNotify(int downLoadMode, int dataType, int id, int errType, String errMsg);

    /**
     * 异常数据清除回调通知
     * @param downLoadMode
     * @param dataType
     * @param id
     * @param opCode
     */
    void onDeleteErrorData(int downLoadMode, int dataType, int id, int opCode);

    /**
     * 发起云端数据列表检测回调通知
     * @param downLoadMode
     * @param dataType
     * @param opCode
     */
    void onRequestCheckSuccess(int downLoadMode, int dataType, int opCode);

}
