package com.sgm.navi.hmi.poi.chargingstation;

import com.android.utils.log.Logger;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.ui.base.BaseModel;

import java.util.UUID;

public class ChargingStationReservationModel extends BaseModel<BaseChargingStationReservationViewModel> implements SearchResultCallback {
    private final String mCallbackId;
    private final SearchPackage mSearchPackage;

    public ChargingStationReservationModel(){
        mCallbackId = UUID.randomUUID().toString();
        mSearchPackage = SearchPackage.getInstance();
        mSearchPackage.registerCallBack(mCallbackId,this);
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {

    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mSearchPackage.unRegisterCallBack(mCallbackId);
    }

    @Override
    public void onNetSearchResult(int taskId, String searchKey, BaseRep result) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mCallbackId: " + mCallbackId + "currentCallbackId: " +mSearchPackage.getCurrentCallbackId());
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            switch (searchKey) {
                case AutoMapConstant.NetSearchKey.QUERY_EQUIPMENT_INFO:
                    mViewModel.onQueryEquipmentResult(taskId,result);
                    break;
                case AutoMapConstant.NetSearchKey.UNLOCK_GROUND:
                    mViewModel.onLockGround(taskId,result);
                    break;
                case AutoMapConstant.NetSearchKey.UPDATE_RESERVATION:
                    mViewModel.onCancelReservation(taskId,result);
                    break;
                case AutoMapConstant.NetSearchKey.QUERY_RESERVATION:
                    mViewModel.onQueryReservation(taskId,result);
                    break;
            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId);
        }
    }

    @Override
    public void onNetSearchResultError(int taskId, String searchKey, String message) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mCallbackId: " + mCallbackId + "currentCallbackId: " +mSearchPackage.getCurrentCallbackId());
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            switch (searchKey) {
                case AutoMapConstant.NetSearchKey.UNLOCK_GROUND:
                    mViewModel.onLockGroundError(taskId,message);
                    break;
                case AutoMapConstant.NetSearchKey.UPDATE_RESERVATION:
                    mViewModel.onCancelReservationError(taskId,message);
                    break;
                case AutoMapConstant.NetSearchKey.QUERY_EQUIPMENT_INFO:
                case AutoMapConstant.NetSearchKey.QUERY_RESERVATION:
                    mViewModel.onSearchError(taskId,message);
                    break;
            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId);
        }
    }
}
