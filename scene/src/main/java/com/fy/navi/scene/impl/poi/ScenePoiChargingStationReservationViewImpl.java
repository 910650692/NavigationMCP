package com.fy.navi.scene.impl.poi;

import android.app.Activity;

import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.poi.IScenePoiChargingStationReservationView;
import com.fy.navi.scene.ui.poi.ScenePoiChargingStationReservationView;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.ConnectorInfoItem;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ReservationInfo;
import com.fy.navi.service.define.user.account.AccessTokenParam;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;

import java.util.concurrent.CompletableFuture;

public class ScenePoiChargingStationReservationViewImpl extends BaseSceneModel<ScenePoiChargingStationReservationView> implements IScenePoiChargingStationReservationView {
    private final SearchPackage mSearchPackage;
    private final CalibrationPackage mCalibrationPackage;
    public ScenePoiChargingStationReservationViewImpl(ScenePoiChargingStationReservationView mScreenView) {
        super(mScreenView);
        mSearchPackage = SearchPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
    }

    public void closeFragment(){
        mScreenView.closeFragment();
    }

    public CompletableFuture<ETAInfo> getTravelTimeFuture(final GeoPoint geoPoint) {
        return mSearchPackage.getTravelTimeFutureIncludeChargeLeft(geoPoint);
    }

    public void queryEquipmentInfo(ReservationInfo reservationInfo){
        mSearchPackage.queryEquipmentInfo(reservationInfo);
    }

    public void queryReservation(PoiInfoEntity poiInfo,Activity activity){
        AccessTokenParam param = new AccessTokenParam(
                AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
                AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
                null,
                activity,
                null,
                null,
                null,
                null);
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = "2";
            mSearchPackage.queryReservation(poiInfo,vehicleBrand,3,idpUserId,accessToken);
        });
    }

    public void unGroundLock(ConnectorInfoItem item, PoiInfoEntity poiInfo, Activity activity){
        AccessTokenParam param = new AccessTokenParam(
                AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
                AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
                null,
                activity,
                null,
                null,
                null,
                null);
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = String.valueOf(mCalibrationPackage.brand());
            mSearchPackage.unGroundLock(item, poiInfo,idpUserId,accessToken,vehicleBrand);
        });
    }

    public void cancelReservation(ReservationInfo reservationInfo,Activity activity){
        AccessTokenParam param = new AccessTokenParam(
                AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
                AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
                null,
                activity,
                null,
                null,
                null,
                null);
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            mSearchPackage.cancelReservation(reservationInfo,idpUserId,accessToken);
        });
    }
}
