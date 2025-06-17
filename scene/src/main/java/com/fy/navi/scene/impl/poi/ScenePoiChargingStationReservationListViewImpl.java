package com.fy.navi.scene.impl.poi;

import android.app.Activity;
import android.util.Log;

import androidx.lifecycle.MutableLiveData;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.poi.IScenePoiChargingStationReservationListView;
import com.fy.navi.scene.ui.poi.ScenePoiChargingStationReservationListView;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.ChargeEquipmentInfo;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ConnectorInfoItem;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ReservationInfo;
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
    private int mTaskId;
    public ScenePoiChargingStationReservationListViewImpl(ScenePoiChargingStationReservationListView mScreenView) {
        super(mScreenView);
        searchType = new MutableLiveData<>(1);
        mSearchPackage = SearchPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
    }
    public void closeFragment(){
        mScreenView.closeFragment();
    }

    public int getMTaskId() {
        return mTaskId;
    }

    public void queryEquipmentInfo(EquipmentInfo info,PoiInfoEntity poiInfo){
        if(ConvertUtils.isNull(info) || ConvertUtils.isNull(poiInfo)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"info or poiInfo is null");
            return;
        }
        mTaskId = mSearchPackage.queryEquipmentInfo(info.getmEquipmentId(),poiInfo.getStationId(),poiInfo.getOperatorId());
    }

    public void createReversion(ConnectorInfoItem item, PoiInfoEntity poiInfo, AccessTokenParam param){
        if(ConvertUtils.isNull(item) || ConvertUtils.isNull(poiInfo) || ConvertUtils.isNull(param)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"info is null");
            return;
        }
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = mSearchPackage.getBrandId(mCalibrationPackage.brand());
            mTaskId = mSearchPackage.createReservation(item, poiInfo,idpUserId,accessToken,vehicleBrand);
        });
    }

    public void unGroundLock(ConnectorInfoItem item, PoiInfoEntity poiInfo, AccessTokenParam param){
        if(ConvertUtils.isNull(item) || ConvertUtils.isNull(poiInfo) || ConvertUtils.isNull(param)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"info is null");
            return;
        }
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = String.valueOf(mCalibrationPackage.brand());
            mTaskId = mSearchPackage.unGroundLock(item, poiInfo,idpUserId,accessToken,vehicleBrand);
        });
    }

    public void queryReservation(PoiInfoEntity poiInfo,AccessTokenParam param,Integer status){
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = mSearchPackage.getBrandId(mCalibrationPackage.brand());
            mTaskId = mSearchPackage.queryReservation(vehicleBrand,status,idpUserId,accessToken);
        });
    }

    public void cancelReservation(ReservationInfo reservationInfo, AccessTokenParam param){
        if(ConvertUtils.isNull(reservationInfo) || ConvertUtils.isNull(param)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"reservationInfo or activity is null");
            return;
        }
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            mTaskId = mSearchPackage.cancelReservation(reservationInfo.getmPreNum(),idpUserId,accessToken);
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

    public void createTimeTick(GeoPoint destPoint){
        mSearchPackage.createTimeTick(destPoint);
    }

    public void setReservationPreNum(String preNum){
        mSearchPackage.setReservationPreNum(preNum);
    }
}
