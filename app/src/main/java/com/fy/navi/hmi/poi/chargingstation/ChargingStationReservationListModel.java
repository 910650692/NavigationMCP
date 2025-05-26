package com.fy.navi.hmi.poi.chargingstation;

import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

import java.util.UUID;

public class ChargingStationReservationListModel extends BaseModel<BaseChargingStationReservationListViewModel>  implements SearchResultCallback {
    private final String mCallbackId;
    private final SearchPackage mSearchPackage;
    public ChargingStationReservationListModel(){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "ChargingStationReservationListModel 初始化");
        mSearchPackage = SearchPackage.getInstance();
        mCallbackId = UUID.randomUUID().toString();
        mSearchPackage.registerCallBack(mCallbackId,this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "unRegisterCallBack");
        mSearchPackage.unRegisterCallBack(mCallbackId);
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {

    }

    @Override
    public void onNetSearchResult(int taskId, String searchKey,BaseRep result) {
        switch (searchKey){
            case AutoMapConstant.NetSearchKey.CREATE_RESERVATION:
                mViewModel.onCreateReservationResult(result);
                break;
            case AutoMapConstant.NetSearchKey.QUERY_EQUIPMENT_INFO:
                mViewModel.onQueryEquipmentResult(result);
                break;
            case AutoMapConstant.NetSearchKey.UNLOCK_GROUND:
                mViewModel.onUnLockResult(result);
                break;
            case AutoMapConstant.NetSearchKey.QUERY_RESERVATION:
                mViewModel.onQueryReservation(result);
                break;
            case AutoMapConstant.NetSearchKey.UPDATE_RESERVATION:
                mViewModel.onCancelReservation(result);
                break;
        }
    }
}
