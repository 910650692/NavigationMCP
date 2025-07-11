package com.sgm.navi.hmi.traffic;

import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.define.aos.FyCriticism;
import com.sgm.navi.service.define.aos.FyGSubTraEventDetail;
import com.sgm.navi.service.define.aos.FyGTraEventDetail;
import com.sgm.navi.service.define.aos.FyTrafficUploadParameter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.aos.AosRestrictedPackage;
import com.sgm.navi.service.logicpaket.aos.IAosRestrictedObserver;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.ui.base.BaseModel;

/**
 * Author: QiuYaWei
 * Date: 2025/2/27
 * Description: [在这里描述文件功能]
 */
public class TrafficModel extends BaseModel<BaseTrafficViewModel> implements IAosRestrictedObserver {
    private static final String TAG = "TrafficModel";
    private AosRestrictedPackage aosRestrictedPackage;
    private LayerPackage layerPackage;
    private PositionPackage positionPackage;
    private long taskId;

    @Override
    public void onCreate() {
        super.onCreate();
        aosRestrictedPackage = AosRestrictedPackage.getInstance();
        layerPackage = LayerPackage.getInstance();
        positionPackage = PositionPackage.getInstance();
        aosRestrictedPackage.addRestrictedObserver(TAG, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        aosRestrictedPackage.removeRestrictedObserver(TAG);
    }

    public double calcStraightDistance(GeoPoint endPoint) {
        return layerPackage.calcStraightDistance(positionPackage.currentGeo, endPoint);
    }

    @SuppressWarnings("FORWARD_NULL")
    @Override
    public void queryTrafficEventDetailResult(@Nullable final FyGTraEventDetail detail) {
        IAosRestrictedObserver.super.queryTrafficEventDetailResult(detail);
        Logger.i(TAG, "queryTrafficEventDetailResult result:" + (detail != null));
        if (taskId == detail.taskId) {
            Logger.i(TAG, "is  my request");
            ThreadManager.getInstance().postUi(() -> {
                mViewModel.updateUi(detail, false);
            });
        }
    }

    @Override
    public void onTrafficUploadFinished(boolean isSuccess) {
        IAosRestrictedObserver.super.onTrafficUploadFinished(isSuccess);
        mViewModel.onTrafficUploadFinished(isSuccess);
    }

    @Override
    public void onDynamicPraiseQueryFinished(FyCriticism fyCriticism) {
        IAosRestrictedObserver.super.onDynamicPraiseQueryFinished(fyCriticism);
        // 查询点赞结果，如果未成功或者失败都不可点击
        mViewModel.updatePrimaseStatus(fyCriticism);
    }

    public void queryTrafficEventInfo(PoiInfoEntity entity, boolean isNeedConvert) {
        Logger.i(TAG, "queryTrafficEventInfo");
        if (aosRestrictedPackage != null) {
            taskId = aosRestrictedPackage.queryTrafficEventInfo(entity.getPid(), isNeedConvert);
        }
    }

    public void queryDynamicPraise(String eventId) {
        Logger.i(TAG, "queryDynamicPraise:" + eventId);
        if (aosRestrictedPackage != null && eventId != null) {
            aosRestrictedPackage.queryTrafficPraiseInfo(eventId);
        }
    }

    public void uploadTrafficEvent(boolean isReal, FyGSubTraEventDetail fyGTraEventDetail) {
        if (fyGTraEventDetail == null) return;
        FyTrafficUploadParameter fyTrafficUploadParameter = new FyTrafficUploadParameter();
        fyTrafficUploadParameter.action = 5;//1 - 创建 4 - 更新 5 - 恢复
        fyTrafficUploadParameter.lon = fyGTraEventDetail.fLon;
        fyTrafficUploadParameter.lat = fyGTraEventDetail.fLat;
        fyTrafficUploadParameter.type = isReal ? 1 : 0;
        fyTrafficUploadParameter.eventId = fyGTraEventDetail.id;
        aosRestrictedPackage.updateTrafficEvent(fyTrafficUploadParameter);
    }
}
