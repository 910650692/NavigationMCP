package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.scene.databinding.SceneNaviDriveReportViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviDriveReportImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;

/**
 * 形成结束后的形成报告页面
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviDriveReportView extends NaviSceneBase<SceneNaviDriveReportViewBinding,
        SceneNaviDriveReportImpl> {

    public SceneNaviDriveReportView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviDriveReportView(@NonNull final Context context,
                                    @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviDriveReportView(@NonNull final Context context,
                                    @Nullable final AttributeSet attrs,
                                    final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_DRIVE_REPORT;
    }

    @Override
    protected SceneNaviDriveReportViewBinding createViewBinding(final LayoutInflater inflater,
                                                        final ViewGroup viewGroup) {
        return SceneNaviDriveReportViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviDriveReportImpl initSceneImpl() {
        return new SceneNaviDriveReportImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviDriveReport(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    /**
     * @param entity 驾驶报告
     */
    public void onDriveReport(final NaviDriveReportEntity entity) {
        mScreenViewModel.onDriveReport(entity);
    }
}
