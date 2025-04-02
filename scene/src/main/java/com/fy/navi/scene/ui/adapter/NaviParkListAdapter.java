
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
import com.fy.navi.scene.api.navi.INaviParkItemClickListener;
import com.fy.navi.scene.databinding.SceneNaviParkListItemBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;

public class NaviParkListAdapter extends RecyclerView.Adapter<NaviParkListAdapter.ResultHolder> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final List<NaviParkingEntity> mList;
    private INaviParkItemClickListener onItemClickListener;
    private int mSelectIndex;

    public void setOnItemClickListener(INaviParkItemClickListener listener) {
        onItemClickListener = listener;
    }

    public NaviParkListAdapter() {
        this.mList = new ArrayList<>();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void notifyList(List<NaviParkingEntity> list, int select) {
        Logger.d(TAG, "NaviAddViaAdapter notifyList " + list);
        if (ConvertUtils.isEmpty(list)) {
            return;
        }
        mSelectIndex = select;
        mList.clear();
        mList.addAll(list);
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        SceneNaviParkListItemBinding itemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.scene_navi_park_list_item, parent, false);
        return new ResultHolder(itemBinding);
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull ResultHolder holder, @SuppressLint("RecyclerView") int position) {
        Logger.d(TAG, "NaviAddViaAdapter onBindViewHolder " + position + ",mSelectIndex= " + mSelectIndex);
        NaviParkingEntity naviParkingEntity = mList.get(position);
        holder.itemBinding.setParkBean(naviParkingEntity);
//        holder.itemBinding.sivParkingEnd.setVisibility((mList.size() == 1 && naviParkingEntity.isEndPoi()) ? View.VISIBLE : View.GONE);
        holder.itemBinding.stvNum.setText(position + 1 + "");
        holder.itemBinding.sclListItem.setSelected(mSelectIndex == position);
        if (!ConvertUtils.isEmpty(naviParkingEntity.getTag())) {
            holder.itemBinding.stvParkingState.setTextColor((naviParkingEntity.getTag().equals(AppContext.getInstance().getMContext().getString(R.string.navi_recommend_parking_adequate))) ?
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.navi_color_C73333_100) :
                    AppContext.getInstance().getMContext().getResources().getColor(R.color.navi_color_2461EA_100));
        }
        holder.itemBinding.getRoot().setOnClickListener(v -> {
            Logger.d(TAG, "NaviAddViaAdapter item click " + position);
            if (onItemClickListener != null) {
                onItemClickListener.onItemClick(mList.size(), position, mList.get(position));
            }
        });
        holder.itemBinding.svParkingNavi.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (onItemClickListener != null) {
                    onItemClickListener.onNaviClick(position, mList.get(position));
                }
            }
        });
    }

    @Override
    public int getItemCount() {
        Logger.d(TAG, "NaviAddViaAdapter getItemCount " + mList.size());
        return mList.size();
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        public SceneNaviParkListItemBinding itemBinding;

        public ResultHolder(SceneNaviParkListItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.itemBinding = resultItemBinding;
            this.itemBinding.setHolder(this);
        }
    }
}