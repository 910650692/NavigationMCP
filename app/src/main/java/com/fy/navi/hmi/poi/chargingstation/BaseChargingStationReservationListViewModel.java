package com.fy.navi.hmi.poi.chargingstation;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseChargingStationReservationListViewModel extends BaseViewModel<ChargingStationReservationListFragment,ChargingStationReservationListModel> {
    public BaseChargingStationReservationListViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected ChargingStationReservationListModel initModel() {
        return new ChargingStationReservationListModel();
    }

    public void onCreateReservationResult(BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getResultCode"+result.getResultCode());
        if("0000".equals(result.getResultCode())){
            mView.notifyCreateReservationSuccess();
        }else{
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getError"+result.getMessage());
        }
    }

    public void onQueryEquipmentResult(BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getResultCode"+result.getResultCode());
        if("0000".equals(result.getResultCode())){
            EquipmentInfo equipmentInfo = GsonUtils.fromJson(String.valueOf(result.getDataSet()), EquipmentInfo.class);
            mView.notifyEquipmentResult(equipmentInfo);
        }else{
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getError"+result.getMessage());
        }
    }

    public void onUnLockResult(BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getResultCode"+result.getResultCode());
        mView.notifyUnLockResult(result.getResultCode());
    }
}
