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

    public void onCreateReservationResult(int taskId,BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getResultCode",result.getResultCode());
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
            try {
                JSONObject jsonObject = new JSONObject(GsonUtils.toJson(result.getDataSet()));
                String preNum = jsonObject.getString("preNum");
                mView.notifyCreateReservationSuccess(taskId,preNum);
            } catch (JSONException e) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error: "+e);
            }
        }else{
            ToastUtils.Companion.getInstance().showCustomToastView(result.getMessage());
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getError",result.getMessage());
        }
    }

    public void onQueryEquipmentResult(int taskId,BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getResultCode",result.getResultCode());
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
            try {
                JSONArray jsonArray = new JSONArray(GsonUtils.toJson(result.getDataSet()));
                EquipmentInfo equipmentInfo = GsonUtils.fromJson(String.valueOf(jsonArray.get(0)), EquipmentInfo.class);
                mView.notifyEquipmentResult(taskId,equipmentInfo);
            } catch (JSONException e) {
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.query_error));
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error: ",e);
            }
        }else{
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.query_error));
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getError",result.getMessage());
        }
    }

    public void onUnLockResult(int taskId,BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getResultCode",result.getResultCode());
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
            mView.notifyUnLockResult(taskId);
        }else{
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.unlock_error));
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getError",result.getMessage());
        }
    }

    public void onQueryReservation(int taskId,BaseRep result){
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "code" , result.getResultCode());
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
                    mView.notifyCancelReservation(taskId,cancelList);
                }else if(!ConvertUtils.isEmpty(PreList)){
                    mView.notifyReadyReservation(taskId,PreList);
                }

            } catch (JSONException e) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error message: "+ e.getMessage());
                onSearchError(taskId,e.getMessage());
            }
        }else{
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG,"onQueryEquipmentResult error");
            onSearchError(taskId,result.getMessage());
        }
    }

    public void onCancelReservation(int taskId,BaseRep result){
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
            mView.notifyCancelSuccess(taskId);
        }else{
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG,"onQueryEquipmentResult error");
            onSearchError(taskId,result.getMessage());
        }
    }

    public void onSearchError(int taskId,String message){
        mView.onSearchError(taskId,message);
    }
}
