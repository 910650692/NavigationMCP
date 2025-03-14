package com.fy.navi.scene.ui.setting;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.CompoundButton;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneSettingPreferenceBinding;
import com.fy.navi.scene.impl.route.SceneRoutePreferenceImpl;
import com.fy.navi.service.define.route.RoutePreferenceID;

public class SceneSettingPreferenceView extends BaseSceneView<SceneSettingPreferenceBinding, SceneRoutePreferenceImpl> implements SceneRoutePreferenceImpl.IRoutePreferenceChangeListener{
    public SceneSettingPreferenceView(@NonNull Context context) {
        super(context);
    }

    public SceneSettingPreferenceView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneSettingPreferenceView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneSettingPreferenceBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneSettingPreferenceBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRoutePreferenceImpl initSceneImpl() {
        return new SceneRoutePreferenceImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        if (mViewBinding != null) {
            mViewBinding.setSceneSettingPreferenceView(mScreenViewModel);
        }
    }

    @Override
    protected void initObserver() {
        mScreenViewModel.setOnPreferenceChangeListener("setting fragment",this);
        mViewBinding.preferenceRecommend.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mViewBinding.preferenceAvoidCongestion.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mViewBinding.preferenceLessCharge.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mViewBinding.preferenceNotHighway.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mViewBinding.preferenceFirstHighway.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mViewBinding.preferenceFirstMainRoad.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mViewBinding.preferenceFastestSpeed.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mScreenViewModel.setDefaultPreference();
    }

    public void updateCheckBoxTextColor(CompoundButton compoundButton, boolean isSelected) {
        if (isSelected) {
            compoundButton.setTextColor(getResources().getColor(R.color.white));
        } else {
            compoundButton.setTextColor(getResources().getColor(R.color.setting_preference_text_gray));
        }
    }

    @Override
    public void onPreferenceChange(RoutePreferenceID routePreference, boolean isFirstChange) {
        mViewBinding.preferenceRecommend.setChecked(mScreenViewModel.ISRECOMMENDSELECT);
        mViewBinding.preferenceAvoidCongestion.setChecked(mScreenViewModel.ISAVOIDCONGESTIONSELECT);
        mViewBinding.preferenceLessCharge.setChecked(mScreenViewModel.ISLESSCHARGESELECT);
        mViewBinding.preferenceNotHighway.setChecked(mScreenViewModel.ISNOTHIGHWAYSELECT);
        mViewBinding.preferenceFirstHighway.setChecked(mScreenViewModel.ISFIRSTHIGHWAYSELECT);
        mViewBinding.preferenceFirstMainRoad.setChecked(mScreenViewModel.ISFIRSTMAINROADSELECT);
        mViewBinding.preferenceFastestSpeed.setChecked(mScreenViewModel.ISFASTESTSPEEDSELECT);
    }
}
