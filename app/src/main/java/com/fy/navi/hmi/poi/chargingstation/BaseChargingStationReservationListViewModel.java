package com.fy.navi.hmi.poi.chargingstation;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.service.define.search.ReservationInfo;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.ui.base.BaseViewModel;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;

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
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
            mView.notifyCreateReservationSuccess();
        }else{
            ToastUtils.Companion.getInstance().showCustomToastView(result.getMessage());
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getError"+result.getMessage());
        }
    }

    public void onQueryEquipmentResult(BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getResultCode"+result.getResultCode());
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
            try {
                JSONArray jsonArray = new JSONArray(GsonUtils.toJson(result.getDataSet()));
                EquipmentInfo equipmentInfo = GsonUtils.fromJson(String.valueOf(jsonArray.get(0)), EquipmentInfo.class);
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
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
            mView.notifyUnLockResult();
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.unlock_success));
        }else{
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.unlock_error));
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getError"+result.getMessage());
        }
    }

    public void onQueryReservation(BaseRep result){
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "code" + result.getResultCode());
            ArrayList<ReservationInfo> cancelList = new ArrayList<>();
            ArrayList<ReservationInfo> PreList = new ArrayList<>();
            // 回调出的数据转换List
            try {
                JSONObject jsonObject = new JSONObject(GsonUtils.toJson(result.getDataSet()));
                JSONArray jsonArray = jsonObject.getJSONArray("resultList");
                String userId = AccountPackage.getInstance().getUserId();
                for (int i = 0; i < jsonArray.length(); i++) {
                    ReservationInfo reservationInfo = GsonUtils.fromJson(String.valueOf(jsonArray.get(i)), ReservationInfo.class);
                    if (reservationInfo.getmUserId().equals(userId) && reservationInfo.getmStatus() == 3) {
                        cancelList.add(reservationInfo);
                    }
                    if (reservationInfo.getmUserId().equals(userId) && reservationInfo.getmStatus() == 1) {
                        PreList.add(reservationInfo);
                    }
                }
                if(!ConvertUtils.isEmpty(cancelList)){
                    mView.notifyCancelReservation(cancelList);
                }else if(!ConvertUtils.isEmpty(PreList)){
                    mView.notifyReadyReservation(PreList);
                }

            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        }else{
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG,"onQueryEquipmentResult error");
        }
    }

    public void onCancelReservation(BaseRep result){
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
            mView.notifyCancelSuccess();
        }else{
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.query_error));
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG,"onQueryEquipmentResult error");
        }
    }
}
