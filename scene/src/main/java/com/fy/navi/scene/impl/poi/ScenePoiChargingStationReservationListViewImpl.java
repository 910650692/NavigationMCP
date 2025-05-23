package com.fy.navi.scene.impl.poi;

import android.app.Activity;

import androidx.lifecycle.MutableLiveData;

import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.poi.IScenePoiChargingStationReservationListView;
import com.fy.navi.scene.ui.poi.ScenePoiChargingStationReservationListView;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.ChargeEquipmentInfo;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ConnectorInfoItem;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.user.account.AccessTokenParam;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.ui.base.StackManager;

import java.util.ArrayList;
import java.util.Objects;

public class ScenePoiChargingStationReservationListViewImpl extends BaseSceneModel<ScenePoiChargingStationReservationListView> implements IScenePoiChargingStationReservationListView {
    public MutableLiveData<Integer> searchType = new MutableLiveData<>();
    public MutableLiveData<ChargeInfo> mChargeInfo = new MutableLiveData<>();
    public MutableLiveData<PoiInfoEntity> mPoiInfo = new MutableLiveData<>();
    private final SearchPackage mSearchPackage;
    private final CalibrationPackage mCalibrationPackage;
    public ScenePoiChargingStationReservationListViewImpl(ScenePoiChargingStationReservationListView mScreenView) {
        super(mScreenView);
        searchType = new MutableLiveData<>(1);
        mSearchPackage = SearchPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
    }
    public void closeFragment(){
        mScreenView.closeFragment();
    }

    public void queryEquipmentInfo(EquipmentInfo info,PoiInfoEntity poiInfo){
        mSearchPackage.queryEquipmentInfo(info,poiInfo);
    }

    public void createReversion(ConnectorInfoItem item, PoiInfoEntity poiInfo, Activity activity){
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
            String vehicleBrand = mSearchPackage.getBrandId(mCalibrationPackage.brand());
            mSearchPackage.createReservation(item, poiInfo,idpUserId,accessToken,vehicleBrand);
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

    public boolean isHideCanReversion(){
        boolean isHideCanReversion = false;
        ArrayList<ConnectorInfoItem> list = new ArrayList<>();
        if(!ConvertUtils.isNull(mChargeInfo.getValue())){
            for (int i = 0; i < mChargeInfo.getValue().getEquipmentInfo().size(); i++) {
                ConnectorInfoItem item = mChargeInfo.getValue().getEquipmentInfo().get(i).getmConnectorInfoItem().get(0);
                if("1".equals(item.getmStatus()) && item.getmParkingLockFlag() == 1){
                    list.add(item);
                }
            }
            isHideCanReversion = list.isEmpty();
        }
        return isHideCanReversion;
    }
}
