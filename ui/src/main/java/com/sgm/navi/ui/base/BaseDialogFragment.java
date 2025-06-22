package com.sgm.navi.ui.base;

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

public abstract class BaseDialogFragment<V extends ViewDataBinding, VM extends BaseViewModel>
        extends DialogFragment implements IBaseView {
    private final static String TAG = BaseDialogFragment.class.getSimpleName();
    protected V mBinding;
    protected VM mViewModel;
    protected BaseActivity mActivity;
    protected String mScreenId;

    public BaseDialogFragment() {
        if(Logger.openLog) {
            Logger.i(TAG, "onCreate before");
        }
        onCreateBefore();
    }

    @Override
    public void onAttach(final @NonNull Context context) {
        super.onAttach(context);
        mActivity = (BaseActivity) context;
        mScreenId = mActivity.mScreenId;
    }

    @Override
    public void onCreate(final @Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if(Logger.openLog) {
            Logger.i(TAG, "onCreate start");
        }
        createViewModel();
        if(Logger.openLog) {
            Logger.i(TAG, "onCreate end");
        }
    }

    @Nullable
    @Override
    public View onCreateView(final @NonNull LayoutInflater inflater, final @Nullable ViewGroup container, final @Nullable Bundle savedInstanceState) {
        if(Logger.openLog) {
            Logger.i(TAG, "onCreateView start");
        }
        mBinding = DataBindingUtil.inflate(inflater, onLayoutId(), container, false);
        bindViewModel();
        onInitView();
        final View rootView = mBinding.getRoot();
        if(Logger.openLog) {
            Logger.i(TAG, "onCreateView end");
        }
        return rootView;
    }

    @Override
    public void onViewCreated(final @NonNull View view, final @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        if(Logger.openLog) {
            Logger.i(TAG, "onViewCreated start");
        }
        onInitObserver();
        onInitData();
        if(Logger.openLog) {
            Logger.i(TAG, "onViewCreated end");
        }
    }

    @Override
    public void onStart() {
        super.onStart();
        if(Logger.openLog) {
            Logger.i(TAG, "onStart");
        }
    }

    @Override
    public void onResume() {
        super.onResume();
        if(Logger.openLog) {
            Logger.i(TAG, "onResume");
        }
    }

    @Override
    public void onPause() {
        super.onPause();
        if(Logger.openLog) {
            Logger.i(TAG, "onPause");
        }
    }

    @Override
    public void onStop() {
        super.onStop();
        if(Logger.openLog) {
            Logger.i(TAG, "onStop");
        }
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        if(Logger.openLog) {
            Logger.i(TAG, "onDestroyView");
        }
    }

    @Override
    public void onDetach() {
        super.onDetach();
        if(Logger.openLog) {
            Logger.i(TAG, "onDetach");
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if(Logger.openLog) {
            Logger.i(TAG, "onDestroy");
        }
        mActivity = null;
    }

    @Override
    public void closeFragment(final boolean nextShow) {
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
    public void closeAllFragmentsUntilTargetFragment(final String targetFragmentClassName) {
        mActivity.closeAllFragmentsUntilTargetFragment(targetFragmentClassName);
    }

    @Override
    public void closeAllFragmentAndSearchView() {
    }

    @Override
    public void showCurrentFragment() {
        mActivity.showCurrentFragment();
    }

    protected void onNewIntent(final Bundle bundle) {

    }


    protected VM initViewModel() {
        return null;
    }

    /**
     * 创建ViewModel
     */
    private void createViewModel() {
        mViewModel = initViewModel();
        if (mViewModel == null) {
            final Class modelClass;
            final Type type = getClass().getGenericSuperclass();
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

    /**
     * 创建ViewModel
     *
     * @param cls ViewModel
     * @param <T> AndroidViewModel
     * @return AndroidViewModel
     */
    private <T extends AndroidViewModel> T createViewModel(final Class<T> cls) {
        // TODO: 2023/8/17 2.5.0的viewModule暂时先采用这种方式加载
        return new ViewModelProvider(this).get(cls);
    }

    @Override
    public void addFragment(final BaseFragment fragment, final Bundle bundle) {

    }

    /**
     * 绑定ViewModel
     */
    private void bindViewModel() {
        mBinding.setVariable(onInitVariableId(), mViewModel);
        getLifecycle().addObserver(mViewModel);
        mBinding.setLifecycleOwner(this);
        mViewModel.attachView(this, mScreenId);
    }
}