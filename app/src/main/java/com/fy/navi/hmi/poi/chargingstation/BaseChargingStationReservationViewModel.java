package com.fy.navi.hmi.poi.chargingstation;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ReservationInfo;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;

public class BaseChargingStationReservationViewModel extends BaseViewModel<ChargingStationReservationFragment,ChargingStationReservationModel> {
    public BaseChargingStationReservationViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected ChargingStationReservationModel initModel() {
        return new ChargingStationReservationModel();
    }

    private final Action mRootClick = () -> {

    };

    public Action getRootClick() {
        return mRootClick;
    }

    public void onQueryEquipmentResult(BaseRep result){
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
            try {
                JSONArray jsonArray = new JSONArray(GsonUtils.toJson(result.getDataSet()));
                EquipmentInfo equipmentInfo = GsonUtils.fromJson(String.valueOf(jsonArray.get(0)), EquipmentInfo.class);
                mView.notifyEquipmentInfo(equipmentInfo);
            } catch (JSONException e) {
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.query_error));
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error: "+e);
            }
        }else{
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.query_error));
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG,"onQueryEquipmentResult error");
        }
    }

    public void onLockGround(BaseRep result){
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
            mView.notifyLockGround();
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.unlock_success));
        }else{
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.unlock_error));
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

    public void onQueryReservation(BaseRep result){
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "code" + result.getResultCode());
            ArrayList<ReservationInfo> list = new ArrayList<>();
            // 回调出的数据转换List
            try {
                JSONObject jsonObject = new JSONObject(GsonUtils.toJson(result.getDataSet()));
                JSONArray jsonArray = jsonObject.getJSONArray("resultList");
                String userId = AccountPackage.getInstance().getUserId();
                for (int i = 0; i < jsonArray.length(); i++) {
                    ReservationInfo reservationInfo = GsonUtils.fromJson(String.valueOf(jsonArray.get(i)), ReservationInfo.class);
                    if (reservationInfo.getmUserId().equals(userId) && reservationInfo.getmStatus() == 3) {
                        list.add(reservationInfo);
                    }
                }
                mView.notifyCancelReservation(list);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        }else{
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG,"onQueryEquipmentResult error");
        }
    }
}
