package com.fy.navi.scene.ui.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.ItemCardGasBinding;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/2
 * Description: [在这里描述文件功能]
 */
public class NaviGasStationAdapter extends RecyclerView.Adapter<NaviGasStationAdapter.ViewHolder> {
    private static final String TAG = "NaviGasStationAdapter";
    private final ArrayList<PoiInfoEntity> mData = new ArrayList<>();
    private Context mContext;
    private OnItemClickListener mItemClickListener;
    private int mSelectIndex = 0;

    public NaviGasStationAdapter(final Context context, ArrayList<PoiInfoEntity> newData) {
        this.mContext = context;
        this.mData.clear();
        this.mData.addAll(newData);
    }

    public void updateData(final List<PoiInfoEntity> newData) {
        this.mData.clear();
        this.mData.addAll(newData);
        notifyDataSetChanged();
        Logger.i(TAG, "updateData");
    }

    public void setOnItemClickListener(NaviGasStationAdapter.OnItemClickListener onItemClickListener) {
        this.mItemClickListener = onItemClickListener;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ItemCardGasBinding binding = DataBindingUtil.inflate(LayoutInflater.from(mContext), R.layout.item_card_gas, parent, false);
        return new ViewHolder(binding);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        final PoiInfoEntity poiInfo = mData.get(position);
        final ItemCardGasBinding binding = holder.mItemCardChargeBinding;
        binding.stvNum.setText(String.valueOf(position + 1));
        binding.tvTitle.setText(poiInfo.getName());
        binding.tvRecLabel.setVisibility(position == 0 ? View.VISIBLE : View.GONE);
        binding.getRoot().setBackgroundResource(position == mSelectIndex ? R.color.common_item_select_color : R.color.transparent);
        if (!ConvertUtils.isNull(poiInfo)) {
            binding.tvDistance.setText(poiInfo.getDistance());
        }
        final List<GasStationInfo> infos = poiInfo.getStationList();
        Logger.i(TAG, "size:" + (ConvertUtils.isEmpty(infos) ? 0 : infos.size()));
        if (ConvertUtils.isEmpty(infos)) {
            binding.ll95.setVisibility(View.GONE);
            binding.ll92.setVisibility(View.GONE);
        } else if (infos.size() >= 2) {
            binding.ll95.setVisibility(View.VISIBLE);
            binding.ll92.setVisibility(View.VISIBLE);
            final GasStationInfo firstBean = infos.get(0);
            final GasStationInfo secondBean = infos.get(1);
            binding.tv95.setVisibility(View.VISIBLE);
            binding.tv95.setText(firstBean.getType());
            binding.tv95Price.setText(firstBean.getPrice());

            binding.tv92.setVisibility(View.VISIBLE);
            binding.tv92.setText(secondBean.getType());
            binding.tv92Price.setText(secondBean.getPrice());
        } else if (infos.size() == 1) {
            binding.ll95.setVisibility(View.VISIBLE);
            binding.ll92.setVisibility(View.VISIBLE);

            final GasStationInfo firstBean = infos.get(0);
            binding.tv95.setVisibility(View.VISIBLE);
            binding.tv95.setText(firstBean.getType());
            binding.tv95Price.setText(firstBean.getPrice());

            binding.tv92.setVisibility(View.GONE);
            binding.tv92Price.setVisibility(View.GONE);
        }
        binding.getRoot().setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mItemClickListener)) {
                if (position != mSelectIndex) {
                    mSelectIndex = position;
                    notifyDataSetChanged();
                }
                mItemClickListener.onItemSelect(mSelectIndex);
            }
        });
        binding.viewNaviNow.setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mItemClickListener)) {
                mItemClickListener.navi(position);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public int getSelectIndex() {
        return mSelectIndex;
    }

    public List<PoiInfoEntity> getData() {
        return mData;
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        private ItemCardGasBinding mItemCardChargeBinding;

        public ViewHolder(@NonNull ItemCardGasBinding binding) {
            super(binding.getRoot());
            this.mItemCardChargeBinding = binding;
        }
    }

    public interface OnItemClickListener {
        /***
         * 立即导航
         * @param index
         */
        void navi(final int index);

        /***
         * 选中
         * @param index
         */
        void onItemSelect(final int index);
    }
}
