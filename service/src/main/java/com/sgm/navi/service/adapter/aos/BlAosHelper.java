package com.sgm.navi.service.adapter.aos;

import com.android.utils.gson.GsonUtils;
import com.autonavi.gbl.aosclient.model.GTraEventDetail;
import com.sgm.navi.service.define.aos.FyGSubTraEventDetail;
import com.sgm.navi.service.define.aos.FyGTraEventDetail;

import java.util.ArrayList;

/**
 * Author: QiuYaWei
 * Date: 2025/2/27
 * Description: [tools]
 */
public class BlAosHelper {
    private BlAosHelper() {
    }

    private static final class InstanceHolder {
        private static final BlAosHelper instance = new BlAosHelper();
    }

    public static BlAosHelper getInstance() {
        return InstanceHolder.instance;
    }

    public FyGTraEventDetail ampConvertMine(GTraEventDetail gTraEventDetail, long taskId) {
        ArrayList<FyGSubTraEventDetail> fyGSubTraEventDetails = new ArrayList<>();
        gTraEventDetail.subinfo.forEach((gSubTraEventDetail -> {
            FyGSubTraEventDetail fyGSubTraEventDetail = GsonUtils.convertToT(gSubTraEventDetail, FyGSubTraEventDetail.class);
            fyGSubTraEventDetails.add(fyGSubTraEventDetail);
        }));
        FyGTraEventDetail detail = GsonUtils.convertToT(gTraEventDetail, FyGTraEventDetail.class);
        detail.taskId = taskId;
        detail.isRequestSuccess = true;
        return detail;
    }
}
