package com.sgm.navi.scene.impl.poi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.poi.IScenePoiChargingStationReservationView;
import com.sgm.navi.scene.ui.poi.ScenePoiChargingStationReservationView;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.ConnectorInfoItem;
import com.sgm.navi.service.define.search.ETAInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.ReservationInfo;
import com.sgm.navi.service.define.user.account.AccessTokenParam;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;

import java.util.concurrent.CompletableFuture;

public class ScenePoiChargingStationReservationViewImpl extends BaseSceneModel<ScenePoiChargingStationReservationView> implements IScenePoiChargingStationReservationView {
    private final SearchPackage mSearchPackage;
    private final CalibrationPackage mCalibrationPackage;
    private int mTaskId;
    public ScenePoiChargingStationReservationViewImpl(ScenePoiChargingStationReservationView mScreenView) {
        super(mScreenView);
        mSearchPackage = SearchPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
    }

    public int getMTaskId() {
        return mTaskId;
    }

    public void closeFragment(){
        mScreenView.closeFragment();
    }

    public CompletableFuture<ETAInfo> getTravelTimeFuture(final GeoPoint geoPoint) {
        return mSearchPackage.getTravelTimeFutureIncludeChargeLeft(geoPoint);
    }

    public void queryEquipmentInfo(ReservationInfo reservationInfo){
        if(ConvertUtils.isNull(reservationInfo)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"reservationInfo is null");
            return;
        }
        mTaskId = mSearchPackage.queryEquipmentInfo(reservationInfo);
    }

    public void queryReservation(PoiInfoEntity poiInfo,AccessTokenParam param){
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = mSearchPackage.getBrandId(mCalibrationPackage.brand());
            mTaskId = mSearchPackage.queryReservation(vehicleBrand,3,idpUserId,accessToken);
        });
    }

    public void unGroundLock(ConnectorInfoItem item, PoiInfoEntity poiInfo, AccessTokenParam param){
        if(ConvertUtils.isNull(item) || ConvertUtils.isNull(poiInfo) || ConvertUtils.isNull(param)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"reservationInfo or activity is null");
            return;
        }
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = String.valueOf(mCalibrationPackage.brand());
            mTaskId = mSearchPackage.unGroundLock(item, poiInfo,idpUserId,accessToken,vehicleBrand);
        });
    }

    public void cancelReservation(ReservationInfo reservationInfo,AccessTokenParam param){
        if(ConvertUtils.isNull(reservationInfo) || ConvertUtils.isNull(param)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"reservationInfo or activity is null");
            return;
        }
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"PreNum: "+reservationInfo.getmPreNum());
            mTaskId = mSearchPackage.cancelReservation(reservationInfo.getmPreNum(),idpUserId,accessToken);
        });
    }

    public void setReservationPreNum(String preNum){
        mSearchPackage.setReservationPreNum(preNum);
    }
}
