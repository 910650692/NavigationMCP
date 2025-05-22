package com.fy.navi.hmi.poi.chargingstation;

import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

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
        switch (searchKey){
            case AutoMapConstant.NetSearchKey.QUERY_EQUIPMENT_INFO:
                mViewModel.onQueryEquipmentResult(result);
                break;
            case AutoMapConstant.NetSearchKey.UNLOCK_GROUND:
                mViewModel.onLockGround(result);
            case AutoMapConstant.NetSearchKey.UPDATE_RESERVATION:
                mViewModel.onCancelReservation(result);
            case AutoMapConstant.NetSearchKey.QUERY_RESERVATION:
                mViewModel.onQueryReservation(result);
        }
    }
}
