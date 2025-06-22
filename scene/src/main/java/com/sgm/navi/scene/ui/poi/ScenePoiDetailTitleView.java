package com.sgm.navi.scene.ui.poi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.databinding.ScenePoiDetailsTitleBarBinding;
import com.sgm.navi.scene.impl.poi.ScenePoiDetailTitleImpl;
import com.sgm.navi.service.define.search.PoiInfoEntity;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class ScenePoiDetailTitleView extends BaseSceneView<ScenePoiDetailsTitleBarBinding, ScenePoiDetailTitleImpl> {
    public ScenePoiDetailTitleView(@NonNull final Context context) {
        super(context);
    }

    public ScenePoiDetailTitleView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public ScenePoiDetailTitleView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }


    @Override
    protected ScenePoiDetailsTitleBarBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
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

    /**
     * 开始搜索
     * @param poiInfo poi信息实体类
     */
    public void doSearch(final PoiInfoEntity poiInfo) {
        mScreenViewModel.doSearch(poiInfo);
    }
}
