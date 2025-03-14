
package com.fy.navi.scene.ui.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.navi.INaviViaItemClickListener;
import com.fy.navi.scene.databinding.SceneNaviViaListItemBinding;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviViaEntity;

import java.util.ArrayList;
import java.util.List;

public class NaviViaListAdapter extends RecyclerView.Adapter<NaviViaListAdapter.ResultHolder> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final List<NaviViaEntity> mList;
    private INaviViaItemClickListener onItemClickListener;

    public List<NaviViaEntity> getData() {
        return mList;
    }

    public void setOnItemClickListener(INaviViaItemClickListener listener) {
        onItemClickListener = listener;
    }

    public NaviViaListAdapter() {
        this.mList = new ArrayList<>();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void notifyList(List<NaviViaEntity> list) {
        Logger.d(TAG, "NaviAddViaAdapter notifyList " + list);
        if (ConvertUtils.isEmpty(list)) {
            return;
        }
        mList.clear();
        mList.addAll(list);
        notifyDataSetChanged();
    }

    public void removeData(NaviViaEntity entity) {
        int pos = mList.indexOf(entity);
        if (pos < 0) return;
        this.mList.remove(entity);
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        SceneNaviViaListItemBinding itemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.scene_navi_via_list_item, parent, false);
        return new ResultHolder(itemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull ResultHolder holder, int position) {
        Logger.d(TAG, "NaviAddViaAdapter onBindViewHolder " + position);
        holder.itemBinding.setViaBean(mList.get(position));
        if (position == mList.size() - 1) {
            holder.itemBinding.stvAddViaIcon.setBackgroundResource(R.drawable.img_navi_via_item_btn_end);
            holder.itemBinding.stvAddViaIcon.setText(R.string.navi_via_item_end);
            holder.itemBinding.llActionContainer.setVisibility(View.GONE);
            holder.itemBinding.groupEta.setVisibility(View.GONE);
        } else {
            holder.itemBinding.stvAddViaIcon.setBackgroundResource(R.drawable.img_navi_via_item_btn_pass);
            holder.itemBinding.stvAddViaIcon.setText(R.string.navi_via_item_pass);
            holder.itemBinding.llActionContainer.setVisibility(View.VISIBLE);
            holder.itemBinding.groupEta.setVisibility(View.VISIBLE);
        }
        holder.itemBinding.swipeMenuLayout.setOnClickListener(v -> {
            Logger.d(TAG, "NaviAddViaAdapter item click " + position);
            if (onItemClickListener != null) {
                onItemClickListener.onItemClick(position, mList.get(position));
            }
        });

        holder.itemBinding.llActionContainer.setOnClickListener(v -> {
            Logger.d(TAG, "NaviAddViaAdapter item click del " + position);
            holder.itemBinding.swipeMenuLayout.smoothClose();
            if (onItemClickListener != null) {
                onItemClickListener.onDelClick(position, mList.get(position));
            }
        });
    }

    @Override
    public int getItemCount() {
        Logger.d(TAG, "NaviAddViaAdapter getItemCount " + mList.size());
        return mList.size();
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        public SceneNaviViaListItemBinding itemBinding;

        public ResultHolder(SceneNaviViaListItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.itemBinding = resultItemBinding;
            this.itemBinding.setHolder(this);
        }
    }
}