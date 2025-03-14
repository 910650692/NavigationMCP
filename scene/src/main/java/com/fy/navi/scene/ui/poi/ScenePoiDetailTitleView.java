package com.fy.navi.scene.ui.poi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.databinding.ScenePoiDetailsTitleBarBinding;
import com.fy.navi.scene.impl.poi.ScenePoiDetailTitleImpl;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.PoiInfoEntity;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class ScenePoiDetailTitleView extends BaseSceneView<ScenePoiDetailsTitleBarBinding, ScenePoiDetailTitleImpl> {
    public ScenePoiDetailTitleView(@NonNull Context context) {
        super(context);
    }

    public ScenePoiDetailTitleView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ScenePoiDetailTitleView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }


    @Override
    protected ScenePoiDetailsTitleBarBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return ScenePoiDetailsTitleBarBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected ScenePoiDetailTitleImpl initSceneImpl() {
        return new ScenePoiDetailTitleImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSceneTitleBarView(mScreenViewModel);

    }

    @Override
    protected void initObserver() {

    }

    public void doSearch(PoiInfoEntity poiInfo) {
        mScreenViewModel.doSearch(poiInfo);
    }
}
