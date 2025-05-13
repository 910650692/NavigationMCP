package com.fy.navi.scene.impl.poi;

import androidx.lifecycle.MutableLiveData;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.poi.IScenePoiChargingStationReservationListView;
import com.fy.navi.scene.ui.poi.ScenePoiChargingStationReservationListView;
import com.fy.navi.service.define.search.ChargeEquipmentInfo;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

public class ScenePoiChargingStationReservationListViewImpl extends BaseSceneModel<ScenePoiChargingStationReservationListView> implements IScenePoiChargingStationReservationListView {
    public MutableLiveData<Integer> searchType = new MutableLiveData<>();
    public MutableLiveData<ChargeInfo> mChargeInfo = new MutableLiveData<>();
    public MutableLiveData<PoiInfoEntity> mPoiInfo = new MutableLiveData<>();
    private final SearchPackage mSearchPackage;
    public ScenePoiChargingStationReservationListViewImpl(ScenePoiChargingStationReservationListView mScreenView) {
        super(mScreenView);
        mSearchPackage = SearchPackage.getInstance();
    }
    public void closeFragment(){
        mScreenView.closeFragment();
    }

    public void openChargeQrCodeDialog(){
        mScreenView.openChargeQrCodeDialog();
    }

    public void queryEquipmentInfo(){
        mSearchPackage.queryEquipmentInfo();
    }

    public void createReversion(){
        mSearchPackage.createReservation();
    }
}
