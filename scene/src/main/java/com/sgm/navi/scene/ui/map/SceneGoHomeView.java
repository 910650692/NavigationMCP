package com.sgm.navi.scene.ui.map;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ResourceUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SceneMainGoHomeBinding;
import com.sgm.navi.scene.impl.map.SceneGoHomeImpl;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.route.RouteTMCParam;

public class SceneGoHomeView extends BaseSceneView<SceneMainGoHomeBinding, SceneGoHomeImpl> {

    private RouteTMCParam routeTMCParam;
    private ISceneCallback callback;

    public SceneGoHomeView(@NonNull Context context) {
        super(context);
    }

    public SceneGoHomeView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneGoHomeView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneMainGoHomeBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneMainGoHomeBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneGoHomeImpl initSceneImpl() {
        return new SceneGoHomeImpl(this);
    }

    @Override
    protected void setInitVariableId() {

    }

    public void setCallback(ISceneCallback callback) {
        this.callback = callback;
    }

    @Override
    protected void initObserver() {
        mViewBinding.buttonGoHome.setOnClickListener(v -> {
            if(callback!=null){
                callback.clickGoHomeBtn(routeTMCParam.getMKey() == 0 ? AutoMapConstant.HomeCompanyType.HOME :
                        AutoMapConstant.HomeCompanyType.COMPANY);
            }
        });
    }

    public void setNdGoHomeView(RouteTMCParam routeTMCParam) {
        ThreadManager.getInstance().postUi(() -> {
            //0代表家  1代表公司
            this.routeTMCParam = routeTMCParam;

            mViewBinding.textViewEstimatedArrival.setText(String.format(ResourceUtils.Companion.getInstance().getString(
                    com.sgm.navi.scene.R.string.main_go_arriver_time), routeTMCParam.getMTimeArrive()));
            mViewBinding.skIvBasicHomeProgress.refreshTMC(routeTMCParam.getMRouteLightBarItem());
            if(routeTMCParam.getMKey() == 0){
                mViewBinding.buttonGoHome.setText(ResourceUtils.Companion.getInstance().getString(R.string.main_go_home));
                mViewBinding.textViewTimeRemaining.setText(String.format(ResourceUtils.Companion.getInstance().getString(
                        com.sgm.navi.scene.R.string.main_go_home_text), routeTMCParam.getMTime()));
            }else if(routeTMCParam.getMKey() == 1){
                mViewBinding.buttonGoHome.setText(ResourceUtils.Companion.getInstance().getString(R.string.main_go_company));
                mViewBinding.textViewTimeRemaining.setText(String.format(ResourceUtils.Companion.getInstance().getString(
                        com.sgm.navi.scene.R.string.main_go_company_text), routeTMCParam.getMTime()));
            }
        });
    }
}



