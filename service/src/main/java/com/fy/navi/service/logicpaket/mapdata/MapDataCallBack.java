package com.fy.navi.service.logicpaket.mapdata;


import com.fy.navi.service.define.mapdata.MergedStatusBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

/**
 * @Description
 * @Author fh
 * @date 2024/12/09
 */
public interface MapDataCallBack {

    void onDownLoadStatus(ProvDataInfo provDataInfo);

    void onPercent(ProvDataInfo info);

    void onMergedStatusInfo(MergedStatusBean mergedStatusInfo);

    void onErrorNotify(int downLoadMode, int dataType, int id, int errType, String errMsg);

    void onDeleteErrorData(int downLoadMode, int dataType, int id, int opCode);
}