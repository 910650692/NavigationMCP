package com.fy.navi.ui.base;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.databinding.DataBindingUtil;
import androidx.databinding.ViewDataBinding;
import androidx.fragment.app.DialogFragment;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.ViewModelProvider;

import com.android.utils.log.Logger;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * @Description
 * @Author yaWei
 * @date 2024/11/22
 */
public abstract class BaseDialogFragment<V extends ViewDataBinding, VM extends BaseViewModel>
        extends DialogFragment implements IBaseView {
    private final String TAG = getClass().getSimpleName();
    protected V mBinding;
    protected VM mViewModel;
    protected BaseActivity mActivity;
    protected String mScreenId;

    public BaseDialogFragment() {
        Logger.i(TAG, "onCreate before");
        onCreateBefore();
    }

    @Override
    public void onAttach(@NonNull Context context) {
        super.onAttach(context);
        mActivity = (BaseActivity) context;
        mScreenId = mActivity.mScreenId;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Logger.i(TAG, "onCreate start");
        createViewModel();
        Logger.i(TAG, "onCreate end");
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        Logger.i(TAG, "onCreateView start");
        mBinding = DataBindingUtil.inflate(inflater, onLayoutId(), container, false);
        bindViewModel();
        onInitView();
        View rootView = mBinding.getRoot();
        Logger.i(TAG, "onCreateView end");
        return rootView;
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        Logger.i(TAG, "onViewCreated start");
        onInitObserver();
        onInitData();
        Logger.i(TAG, "onViewCreated end");
    }

    @Override
    public void onStart() {
        super.onStart();
        Logger.i(TAG, "onStart");
    }

    @Override
    public void onResume() {
        super.onResume();
        Logger.i(TAG, "onResume");
    }

    @Override
    public void onPause() {
        super.onPause();
        Logger.i(TAG, "onPause");
    }

    @Override
    public void onStop() {
        super.onStop();
        Logger.i(TAG, "onStop");
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        Logger.i(TAG, "onDestroyView");
    }

    @Override
    public void onDetach() {
        super.onDetach();
        Logger.i(TAG, "onDetach");
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.i(TAG, "onDestroy");
    }

    @Override
    public void closeFragment(boolean nextShow){
        mActivity.closeFragment(nextShow);
    }

    @Override
    public void closeAllFragment() {
        mActivity.closeAllFragment();
    }

    @Override
    public void closeAllFragmentUpRoute() {
        mActivity.closeAllFragmentUpRoute();
    }

    @Override
    public void closeAllFragmentsUntilTargetFragment(String targetFragmentClassName) {
        mActivity.closeAllFragmentsUntilTargetFragment(targetFragmentClassName);
    }

    @Override
    public void closeAllFragmentAndSearchView() {
    }

    protected void onNewIntent(Bundle bundle){

    }


    protected VM initViewModel() {
        return null;
    }

    private void createViewModel() {
        mViewModel = initViewModel();
        if (mViewModel == null) {
            Class modelClass;
            Type type = getClass().getGenericSuperclass();
            if (type instanceof ParameterizedType) {
                // 获取直接继承的父类(也就是BaseActivity本身)的第二个泛型参数Class
                modelClass = (Class) ((ParameterizedType) type).getActualTypeArguments()[1];
            } else {
                // If no generic parameters are specified, BaseViewModel is used by default.
                modelClass = BaseViewModel.class;
            }
            mViewModel = (VM) createViewModel(modelClass);
        }
    }

    private <T extends AndroidViewModel> T createViewModel(Class<T> cls) {
        // TODO: 2023/8/17 2.5.0的viewModule暂时先采用这种方式加载
        return new ViewModelProvider(this).get(cls);
    }

    @Override
    public void addFragment(BaseFragment fragment, Bundle bundle) {

    }

    private void bindViewModel() {
        mBinding.setVariable(onInitVariableId(), mViewModel);
        getLifecycle().addObserver(mViewModel);
        mBinding.setLifecycleOwner(this);
        mViewModel.attachView(this, mScreenId);
    }
}