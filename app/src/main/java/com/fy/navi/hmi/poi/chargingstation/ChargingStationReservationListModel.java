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
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mCallbackId: " + mCallbackId + "currentCallbackId: " +mSearchPackage.getCurrentCallbackId());
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            switch (searchKey) {
                case AutoMapConstant.NetSearchKey.CREATE_RESERVATION:
                    mViewModel.onCreateReservationResult(taskId,result);
                    break;
                case AutoMapConstant.NetSearchKey.QUERY_EQUIPMENT_INFO:
                    mViewModel.onQueryEquipmentResult(taskId,result);
                    break;
                case AutoMapConstant.NetSearchKey.UNLOCK_GROUND:
                    mViewModel.onUnLockResult(taskId,result);
                    break;
                case AutoMapConstant.NetSearchKey.QUERY_RESERVATION:
                    mViewModel.onQueryReservation(taskId,result);
                    break;
                case AutoMapConstant.NetSearchKey.UPDATE_RESERVATION:
                    mViewModel.onCancelReservation(taskId,result);
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
                case AutoMapConstant.NetSearchKey.CREATE_RESERVATION:
                case AutoMapConstant.NetSearchKey.QUERY_EQUIPMENT_INFO:
                case AutoMapConstant.NetSearchKey.UNLOCK_GROUND:
                case AutoMapConstant.NetSearchKey.QUERY_RESERVATION:
                case AutoMapConstant.NetSearchKey.UPDATE_RESERVATION:
                    mViewModel.onSearchError(taskId,message);
                    break;
            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "Ignoring callback for ID: " + mCallbackId);
        }
    }
}
