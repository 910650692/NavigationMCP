package com.fy.navi.hmi.poi.chargingstation;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.ui.base.BaseViewModel;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

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
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.query_error));
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getError"+result.getMessage());
        }
    }

    public void onQueryEquipmentResult(BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getResultCode"+result.getResultCode());
        if("0000".equals(result.getResultCode())){
            try {
                JSONArray jsonArray = new JSONArray(GsonUtils.toJson(result.getDataSet()));
                EquipmentInfo equipmentInfo = GsonUtils.fromJson(GsonUtils.toJson(jsonArray.get(0)), EquipmentInfo.class);
                mView.notifyEquipmentResult(equipmentInfo);
            } catch (JSONException e) {
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.query_error));
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error: "+e);
            }
        }else{
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.query_error));
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getError"+result.getMessage());
        }
    }

    public void onUnLockResult(BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getResultCode"+result.getResultCode());
        if("0000".equals(result.getResultCode())){
            mView.notifyUnLockResult();
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.unlock_error));
        }else{
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.unlock_error));
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getError"+result.getMessage());
        }
    }
}
