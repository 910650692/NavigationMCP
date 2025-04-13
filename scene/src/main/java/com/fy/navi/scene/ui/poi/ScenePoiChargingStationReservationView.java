package com.fy.navi.scene.ui.poi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.databinding.SceneReservationDetailViewBinding;
import com.fy.navi.scene.impl.poi.ScenePoiChargingStationReservationViewImpl;

public class ScenePoiChargingStationReservationView extends BaseSceneView<SceneReservationDetailViewBinding, ScenePoiChargingStationReservationViewImpl> {
    public ScenePoiChargingStationReservationView(@NonNull Context context) {
        super(context);
    }

    public ScenePoiChargingStationReservationView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ScenePoiChargingStationReservationView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneReservationDetailViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneReservationDetailViewBinding.inflate(inflater,viewGroup,true);
    }

    @Override
    protected ScenePoiChargingStationReservationViewImpl initSceneImpl() {
        return new ScenePoiChargingStationReservationViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScenePoiChargingStationReservationView(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    public void closeFragment(){
        closeCurrentFragment();
    }
}
