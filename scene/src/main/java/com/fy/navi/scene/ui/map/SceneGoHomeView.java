package com.fy.navi.scene.ui.map;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.databinding.SceneMainGoHomeBinding;
import com.fy.navi.scene.impl.map.SceneGoHomeImpl;

public class SceneGoHomeView extends BaseSceneView<SceneMainGoHomeBinding, SceneGoHomeImpl> {

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

    @Override
    protected void initObserver() {

    }
}



