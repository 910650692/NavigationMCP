package com.fy.navi.service.adapter.mapdata;

import com.fy.navi.service.define.mapdata.MergedStatusBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/09
 */
public interface MapDataAdapterCallBack {
    void onDownLoadStatus(ProvDataInfo provDataInfo);

    void onPercent(ProvDataInfo info);

    void onMergedStatusInfo(MergedStatusBean mergedStatusInfo);

    void onErrorNotify(int downLoadMode, int dataType, int id, int errType, String errMsg);

    void onDeleteErrorData(int downLoadMode, int dataType, int id, int opCode);
}
